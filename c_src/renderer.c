#include "renderer.h"

#include <assert.h>
#include <stdatomic.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "command.h"
#include "device.h"
#include "params.h"
#include "shaders.h"
#include "spirv_reflect.h"
#include "texture.h"
#include "utils.h"

static PFN_vkCreateShadersEXT vkCreateShadersEXT_;
static PFN_vkDestroyShaderEXT vkDestroyShaderEXT_;

static ErlNifResourceType* RENDERER_RES_TYPE = NULL;
static ERL_NIF_TERM ATOM_RENDERER_HANDLE;

static atomic_uint_fast32_t g_next_params_index = 0;
static atomic_uint_fast32_t g_next_material_index = 0;

static uint32_t alloc_params_index(void) {
    uint32_t idx = atomic_fetch_add(&g_next_params_index, 1);

    if (idx >= MAX_BINDLESS_RESOURCES) {
        enif_fprintf(stderr, "renderer: MAX_BINDLESS_RESOURCES exceeded (%u)\n",
                     (unsigned)MAX_BINDLESS_RESOURCES);
        return UINT32_MAX;
    }

    return idx;
}

static uint32_t alloc_material_index(void) {
    uint32_t idx = atomic_fetch_add(&g_next_material_index, 1);

    if (idx >= MAX_BINDLESS_RESOURCES) {
        enif_fprintf(stderr, "renderer: MAX_BINDLESS_RESOURCES exceeded (%u)\n",
                     (unsigned)MAX_BINDLESS_RESOURCES);
        return UINT32_MAX;
    }

    return idx;
}

static int reflect_ubo_size(const uint32_t* code, size_t code_size,
                            uint32_t binding, size_t* out_size) {
    SpvReflectShaderModule module;

    if (spvReflectCreateShaderModule(code_size, code, &module) !=
        SPV_REFLECT_RESULT_SUCCESS)
        return 0;

    uint32_t count = 0;
    spvReflectEnumerateDescriptorBindings(&module, &count, NULL);

    SpvReflectDescriptorBinding** bindings = malloc(sizeof(*bindings) * count);

    spvReflectEnumerateDescriptorBindings(&module, &count, bindings);

    int found = 0;

    const uint32_t target_set = 0;

    for (uint32_t i = 0; i < count; i++) {
        SpvReflectDescriptorBinding* b = bindings[i];

        if (b->descriptor_type == SPV_REFLECT_DESCRIPTOR_TYPE_UNIFORM_BUFFER &&
            b->set == target_set && b->binding == binding) {
            *out_size = b->block.size;
            found = 1;
            break;
        }
    }

    free(bindings);
    spvReflectDestroyShaderModule(&module);
    return found;
}

static size_t reflect_max_ubo_size(const uint32_t* vert_code, size_t vert_size,
                                   const uint32_t* frag_code, size_t frag_size,
                                   uint32_t binding) {
    size_t vs = 0, fs = 0;

    int v_ok = reflect_ubo_size(vert_code, vert_size, binding, &vs);
    int f_ok = reflect_ubo_size(frag_code, frag_size, binding, &fs);

    if (!v_ok) vs = 0;
    if (!f_ok) fs = 0;

    return (vs > fs) ? vs : fs;
}

static int create_and_bind_ubo(GpuBuffer* out_buf, uint32_t* out_slot,
                               VkDeviceSize ubo_size, uint32_t binding,
                               uint32_t (*alloc_slot_fn)(void)) {
    if (!out_buf || !out_slot || !alloc_slot_fn) return 0;
    if (ubo_size == 0) return 0;

    const VkBufferUsageFlags ubo_usage = VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT;
    const VkMemoryPropertyFlags ubo_mem = VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT |
                                          VK_MEMORY_PROPERTY_HOST_COHERENT_BIT;

    if (!create_gpu_buffer(out_buf, ubo_size, ubo_usage, ubo_mem)) {
        *out_buf = (GpuBuffer){0};
        return 0;
    }

    uint32_t slot = alloc_slot_fn();
    if (slot == UINT32_MAX) {
        destroy_gpu_buffer(out_buf);
        *out_buf = (GpuBuffer){0};
        return 0;
    }

    VkDescriptorBufferInfo bi = {
        .buffer = out_buf->buffer,
        .offset = 0,
        .range = ubo_size,
    };

    VkWriteDescriptorSet w = {
        .sType = VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET,
        .dstSet = g_device.descriptor_set,
        .dstBinding = binding,
        .dstArrayElement = slot,
        .descriptorCount = 1,
        .descriptorType = VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,
        .pBufferInfo = &bi,
    };

    vkUpdateDescriptorSets(g_device.logical_device, 1, &w, 0, NULL);

    *out_slot = slot;
    return 1;
}

static void renderer_res_dtor(ErlNifEnv* env, void* obj) {
    (void)env;
    renderer_res_t* r = (renderer_res_t*)obj;

    vkDeviceWaitIdle(g_device.logical_device);

    for (uint32_t i = 0; i < FRAMES_IN_FLIGHT; i++) {
        destroy_gpu_buffer(&r->frame_params_ubo[i]);
        r->frame_params_index[i] = UINT32_MAX;
    }

    destroy_gpu_buffer(&r->material_ubo);
    r->material_index = UINT32_MAX;

    if (r->frag_shader) {
        vkDestroyShaderEXT_(g_device.logical_device, r->frag_shader, NULL);
        r->frag_shader = (VkShaderEXT)0;
    }

    if (r->vert_shader) {
        vkDestroyShaderEXT_(g_device.logical_device, r->vert_shader, NULL);
        r->vert_shader = (VkShaderEXT)0;
    }
}

int nif_init_renderer_res(ErlNifEnv* env) {
    RENDERER_RES_TYPE = enif_open_resource_type(
        env, "fluo_nif", "renderer_res", renderer_res_dtor,
        ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL);

    if (!RENDERER_RES_TYPE) return -1;

    ATOM_RENDERER_HANDLE = enif_make_atom(env, "renderer_handle");

    vkCreateShadersEXT_ = (PFN_vkCreateShadersEXT)vkGetDeviceProcAddr(
        g_device.logical_device, "vkCreateShadersEXT");

    vkDestroyShaderEXT_ = (PFN_vkDestroyShaderEXT)vkGetDeviceProcAddr(
        g_device.logical_device, "vkDestroyShaderEXT");

    if (!vkCreateShadersEXT_ || !vkDestroyShaderEXT_) {
        enif_fprintf(stderr,
                     "failed to load vkCreateShadersEXT/vkDestroyShaderEXT. ");
        return -1;
    }

    return 0;
}

renderer_res_t* get_renderer_from_term(ErlNifEnv* env, ERL_NIF_TERM term) {
    const ERL_NIF_TERM* elems = NULL;
    int arity = 0;

    if (!enif_get_tuple(env, term, &arity, &elems) || arity != 2) return NULL;

    if (!enif_is_identical(elems[0], ATOM_RENDERER_HANDLE)) return NULL;

    renderer_res_t* res = NULL;
    if (!enif_get_resource(env, elems[1], RENDERER_RES_TYPE, (void**)&res))
        return NULL;

    return res;
}

ERL_NIF_TERM nif_create_renderer(ErlNifEnv* env, int argc,
                                 const ERL_NIF_TERM argv[]) {
    if (argc != 3) return enif_make_badarg(env);

    char* vert_path = NULL;
    char* frag_path = NULL;

    uint8_t* vert_code = NULL;
    uint8_t* frag_code = NULL;
    size_t vert_size = 0, frag_size = 0;

    uint8_t* ubo_blob = NULL;

    renderer_res_t* res = NULL;

    VkShaderEXT shaders_out[2] = {(VkShaderEXT)0, (VkShaderEXT)0};

    if (!get_c_string_from_gleam_string(env, argv[1], &vert_path)) goto fail;
    if (!get_c_string_from_gleam_string(env, argv[2], &frag_path)) goto fail;

    vert_code = read_shader(vert_path, &vert_size);
    frag_code = read_shader(frag_path, &frag_size);

    free(vert_path);
    vert_path = NULL;

    free(frag_path);
    frag_path = NULL;

    if (!vert_code || !frag_code) goto fail;
    if ((vert_size % 4) != 0 || (frag_size % 4) != 0) goto fail;

    res = (renderer_res_t*)enif_alloc_resource(RENDERER_RES_TYPE, sizeof(*res));

    if (!res) goto fail;

    res->vert_shader = (VkShaderEXT)0;
    res->frag_shader = (VkShaderEXT)0;
    res->material_index = UINT32_MAX;
    res->material_ubo = (GpuBuffer){0};

    for (uint32_t i = 0; i < FRAMES_IN_FLIGHT; i++) {
        res->frame_params_ubo[i] = (GpuBuffer){0};
        res->frame_params_index[i] = UINT32_MAX;
    }

    VkPushConstantRange push_range = {
        .stageFlags = VK_SHADER_STAGE_ALL,
        .offset = 0,
        .size = sizeof(PushConstants),
    };

    VkShaderCreateInfoEXT shader_infos[2] = {
        {
            .sType = VK_STRUCTURE_TYPE_SHADER_CREATE_INFO_EXT,
            .stage = VK_SHADER_STAGE_VERTEX_BIT,
            .nextStage = VK_SHADER_STAGE_FRAGMENT_BIT,
            .codeType = VK_SHADER_CODE_TYPE_SPIRV_EXT,
            .codeSize = vert_size,
            .pCode = vert_code,
            .pName = "main",
            .setLayoutCount = 1,
            .pSetLayouts = &g_device.descriptor_layout,
            .pushConstantRangeCount = 1,
            .pPushConstantRanges = &push_range,
        },
        {
            .sType = VK_STRUCTURE_TYPE_SHADER_CREATE_INFO_EXT,
            .stage = VK_SHADER_STAGE_FRAGMENT_BIT,
            .nextStage = 0,
            .codeType = VK_SHADER_CODE_TYPE_SPIRV_EXT,
            .codeSize = frag_size,
            .pCode = frag_code,
            .pName = "main",
            .setLayoutCount = 1,
            .pSetLayouts = &g_device.descriptor_layout,
            .pushConstantRangeCount = 1,
            .pPushConstantRanges = &push_range,
        },
    };

    size_t params_ubo_size = reflect_max_ubo_size(
        (const uint32_t*)vert_code, vert_size, (const uint32_t*)frag_code,
        frag_size, FRAME_PARAMS_BINDING);

    if (params_ubo_size > 0) {
        for (uint32_t i = 0; i < FRAMES_IN_FLIGHT; i++) {
            if (!create_and_bind_ubo(
                    &res->frame_params_ubo[i], &res->frame_params_index[i],
                    (VkDeviceSize)params_ubo_size, FRAME_PARAMS_BINDING,
                    alloc_params_index)) {
                goto fail;
            }
        }
    }

    size_t material_ubo_size = reflect_max_ubo_size(
        (const uint32_t*)vert_code, vert_size, (const uint32_t*)frag_code,
        frag_size, MATERIAL_BINDING);

    if (material_ubo_size > 0) {
        if (!create_and_bind_ubo(&res->material_ubo, &res->material_index,
                                 (VkDeviceSize)material_ubo_size,
                                 MATERIAL_BINDING, alloc_material_index)) {
            goto fail;
        }

        ubo_blob = (uint8_t*)enif_alloc(material_ubo_size);

        if (!ubo_blob) goto fail;

        if (!pack_std140_params_term(env, argv[0], ubo_blob,
                                     material_ubo_size)) {
            goto fail;
        }

        if (!write_gpu_buffer(&res->material_ubo, ubo_blob,
                              (VkDeviceSize)material_ubo_size, 0)) {
            goto fail;
        }

        enif_free(ubo_blob);
        ubo_blob = NULL;
    }

    if (vkCreateShadersEXT_(g_device.logical_device, 2, shader_infos, NULL,
                            shaders_out) != VK_SUCCESS) {
        goto fail;
    }

    res->vert_shader = shaders_out[0];
    res->frag_shader = shaders_out[1];

    ERL_NIF_TERM term = enif_make_resource(env, res);

    enif_release_resource(res);

    return term;

fail:
    if (ubo_blob) enif_free(ubo_blob);
    if (vert_path) free(vert_path);
    if (frag_path) free(frag_path);
    if (vert_code) free(vert_code);
    if (frag_code) free(frag_code);

    if (res) {
        for (uint32_t i = 0; i < FRAMES_IN_FLIGHT; i++) {
            destroy_gpu_buffer(&res->frame_params_ubo[i]);
            res->frame_params_index[i] = UINT32_MAX;
        }

        destroy_gpu_buffer(&res->material_ubo);
        res->material_index = UINT32_MAX;

        if (res->vert_shader) {
            vkDestroyShaderEXT_(g_device.logical_device, res->vert_shader,
                                NULL);
            res->vert_shader = (VkShaderEXT)0;
        }
        if (res->frag_shader) {
            vkDestroyShaderEXT_(g_device.logical_device, res->frag_shader,
                                NULL);
            res->frag_shader = (VkShaderEXT)0;
        }

        enif_release_resource(res);
    }

    return enif_make_badarg(env);
}

uint32_t get_frame_params_index(const renderer_res_t* r, uint32_t frame) {
    if (!r) return UINT32_MAX;
    if (frame >= FRAMES_IN_FLIGHT) return UINT32_MAX;
    return r->frame_params_index[frame];
}

ERL_NIF_TERM nif_set_frame_params(ErlNifEnv* env, int argc,
                                  const ERL_NIF_TERM argv[]) {
    enif_mutex_lock(g_vk_mutex);

    const command_res_t* cmd_res = get_command_from_term(env, argv[0]);

    if (!cmd_res) return enif_make_badarg(env);

    const renderer_res_t* renderer_res = get_renderer_from_term(env, argv[1]);

    if (!renderer_res) return enif_make_badarg(env);

    const uint32_t frame = cmd_res->frame;

    GpuBuffer* buf = (GpuBuffer*)&renderer_res->frame_params_ubo[frame];

    const ERL_NIF_TERM params = argv[2];

    if (!buf || !buf->buffer || buf->size == 0) {
        enif_mutex_unlock(g_vk_mutex);
        return enif_make_atom(env, "ok");
    }

    const size_t ubo_size = (size_t)buf->size;

    uint8_t* ubo_blob = (uint8_t*)buf->mapped;

    if (!ubo_blob) return enif_make_badarg(env);

    if (!pack_std140_params_term(env, params, ubo_blob, ubo_size)) {
        enif_fprintf(
            stderr,
            "renderer: std140 pack failed or size mismatch (size=%llu)\n",
            (unsigned long long)ubo_size);
        return enif_make_badarg(env);
    }

    if (!write_gpu_buffer(buf, ubo_blob, (VkDeviceSize)ubo_size, 0)) {
        enif_fprintf(stderr, "renderer: failed to update params (size=%llu)\n",
                     (unsigned long long)ubo_size);
        return enif_make_badarg(env);
    }

    enif_mutex_unlock(g_vk_mutex);

    return enif_make_atom(env, "ok");
}
