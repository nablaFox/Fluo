#include "renderer.h"

#include <assert.h>
#include <stdatomic.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "device.h"
#include "spirv_reflect.h"
#include "texture.h"
#include "utils.h"

static PFN_vkCreateShadersEXT vkCreateShadersEXT_;
static PFN_vkDestroyShaderEXT vkDestroyShaderEXT_;

static ErlNifResourceType* RENDERER_RES_TYPE = NULL;

static atomic_uint_fast32_t g_next_material_index = 0;

static uint32_t alloc_material_index(void) {
    uint32_t idx = atomic_fetch_add(&g_next_material_index, 1);

    if (idx >= MAX_BINDLESS_RESOURCES) {
        enif_fprintf(stderr, "renderer: MAX_BINDLESS_RESOURCES exceeded (%u)\n",
                     (unsigned)MAX_BINDLESS_RESOURCES);
        return UINT32_MAX;
    }

    return idx;
}

static uint8_t* read_shader(const char* path, size_t* size) {
    const char* prefix = "shaders/";
    const size_t plen = strlen(prefix);
    const size_t path_len = strlen(path);

    char* full = (char*)malloc(plen + path_len + 1);
    assert(full && "failed to allocate full shader path");

    memcpy(full, prefix, plen);
    memcpy(full + plen, path, path_len + 1);

    FILE* f = fopen(full, "rb");
    free(full);

    assert(f && "failed to open shader file");

    fseek(f, 0, SEEK_END);
    *size = (size_t)ftell(f);
    fseek(f, 0, SEEK_SET);

    uint8_t* data = (uint8_t*)malloc(*size);
    assert(data && "failed to malloc shader file buffer");

    size_t n = fread(data, 1, *size, f);
    assert(n == *size && "failed to read full shader file");

    fclose(f);
    return data;
}

static int reflect_ubo_size(const uint32_t* code, size_t code_size,
                            uint32_t set, uint32_t binding, size_t* out_size) {
    SpvReflectShaderModule module;

    if (spvReflectCreateShaderModule(code_size, code, &module) !=
        SPV_REFLECT_RESULT_SUCCESS)
        return 0;

    uint32_t count = 0;
    spvReflectEnumerateDescriptorBindings(&module, &count, NULL);

    SpvReflectDescriptorBinding** bindings = malloc(sizeof(*bindings) * count);

    spvReflectEnumerateDescriptorBindings(&module, &count, bindings);

    int found = 0;

    for (uint32_t i = 0; i < count; i++) {
        SpvReflectDescriptorBinding* b = bindings[i];

        if (b->descriptor_type == SPV_REFLECT_DESCRIPTOR_TYPE_UNIFORM_BUFFER &&
            b->set == set && b->binding == binding) {
            *out_size = b->block.size;
            found = 1;
            break;
        }
    }

    free(bindings);
    spvReflectDestroyShaderModule(&module);
    return found;
}

static void renderer_res_dtor(ErlNifEnv* env, void* obj) {
    (void)env;
    renderer_res_t* r = (renderer_res_t*)obj;

    vkDeviceWaitIdle(g_device.logical_device);

    for (uint32_t i = 0; i < FRAMES_IN_FLIGHT; i++) {
        destroy_gpu_buffer(&r->material_ubo[i]);
        r->material_index[i] = UINT32_MAX;
    }

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

    if (!g_device.logical_device) {
        enif_fprintf(stderr,
                     "renderer: logical device not initialized yet; "
                     "cannot load VK_EXT_shader_object procs\n");
        return -1;
    }

    vkCreateShadersEXT_ = (PFN_vkCreateShadersEXT)vkGetDeviceProcAddr(
        g_device.logical_device, "vkCreateShadersEXT");

    vkDestroyShaderEXT_ = (PFN_vkDestroyShaderEXT)vkGetDeviceProcAddr(
        g_device.logical_device, "vkDestroyShaderEXT");

    if (!vkCreateShadersEXT_ || !vkDestroyShaderEXT_) {
        enif_fprintf(
            stderr,
            "renderer: failed to load vkCreateShadersEXT/vkDestroyShaderEXT. "
            "Is VK_EXT_shader_object enabled and supported?\n");
        return -1;
    }

    return 0;
}

renderer_res_t* get_renderer_from_term(ErlNifEnv* env, ERL_NIF_TERM term) {
    ERL_NIF_TERM handle_term = term;

    const ERL_NIF_TERM* elems = NULL;
    int arity = 0;

    if (enif_get_tuple(env, term, &arity, &elems)) {
        if (arity != 4) return NULL;

        if (!enif_is_atom(env, elems[0])) return NULL;
        if (!enif_is_identical(elems[0], enif_make_atom(env, "renderer")))
            return NULL;

        handle_term = elems[3];
    }

    renderer_res_t* res = NULL;

    if (!enif_get_resource(env, handle_term, RENDERER_RES_TYPE, (void**)&res))
        return NULL;

    return res;
}

static void cleanup_partial_renderer(renderer_res_t* res) {
    if (!res) return;

    for (uint32_t i = 0; i < FRAMES_IN_FLIGHT; i++) {
        destroy_gpu_buffer(&res->material_ubo[i]);
        res->material_index[i] = UINT32_MAX;
    }

    if (res->vert_shader) {
        vkDestroyShaderEXT_(g_device.logical_device, res->vert_shader, NULL);
        res->vert_shader = (VkShaderEXT)0;
    }
    if (res->frag_shader) {
        vkDestroyShaderEXT_(g_device.logical_device, res->frag_shader, NULL);
        res->frag_shader = (VkShaderEXT)0;
    }
}

ERL_NIF_TERM nif_create_renderer(ErlNifEnv* env, int argc,
                                 const ERL_NIF_TERM argv[]) {
    if (argc != 2) return enif_make_badarg(env);

    char* vert_path = NULL;
    char* frag_path = NULL;

    if (!get_c_string_from_gleam_string(env, argv[0], &vert_path))
        return enif_make_badarg(env);

    if (!get_c_string_from_gleam_string(env, argv[1], &frag_path)) {
        free(vert_path);
        return enif_make_badarg(env);
    }

    size_t vert_size = 0, frag_size = 0;
    uint8_t* vert_code = read_shader(vert_path, &vert_size);
    uint8_t* frag_code = read_shader(frag_path, &frag_size);

    free(vert_path);
    free(frag_path);

    if ((vert_size % 4) != 0 || (frag_size % 4) != 0) {
        free(vert_code);
        free(frag_code);
        return enif_make_badarg(env);
    }

    renderer_res_t* res = (renderer_res_t*)enif_alloc_resource(
        RENDERER_RES_TYPE, sizeof(renderer_res_t));

    if (!res) {
        free(vert_code);
        free(frag_code);
        return enif_make_badarg(env);
    }

    res->frag_shader = (VkShaderEXT)0;
    res->vert_shader = (VkShaderEXT)0;

    for (uint32_t i = 0; i < FRAMES_IN_FLIGHT; i++) {
        res->material_ubo[i] = (GpuBuffer){0};
        res->material_index[i] = UINT32_MAX;
    }

    VkPushConstantRange push_range = {
        .stageFlags = VK_SHADER_STAGE_ALL,
        .offset = 0,
        .size = sizeof(PushConstants),
    };

    VkShaderCreateInfoEXT shader_infos[2] = {
        {
            .sType = VK_STRUCTURE_TYPE_SHADER_CREATE_INFO_EXT,
            .pNext = NULL,
            .flags = 0,

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

            .pSpecializationInfo = NULL,
        },
        {
            .sType = VK_STRUCTURE_TYPE_SHADER_CREATE_INFO_EXT,
            .pNext = NULL,
            .flags = 0,

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

            .pSpecializationInfo = NULL,
        },
    };

    VkShaderEXT shaders_out[2] = {(VkShaderEXT)0, (VkShaderEXT)0};

    size_t vert_ubo_size = 0, frag_ubo_size = 0;

    int vert_has_ubo = reflect_ubo_size((const uint32_t*)vert_code, vert_size,
                                        0, UNIFORM_BINDING, &vert_ubo_size);

    int frag_has_ubo = reflect_ubo_size((const uint32_t*)frag_code, frag_size,
                                        0, UNIFORM_BINDING, &frag_ubo_size);

    if (!vert_has_ubo) vert_ubo_size = 0;
    if (!frag_has_ubo) frag_ubo_size = 0;

    size_t ubo_size =
        vert_ubo_size > frag_ubo_size ? vert_ubo_size : frag_ubo_size;

    if (vkCreateShadersEXT_(g_device.logical_device, 2, shader_infos, NULL,
                            shaders_out) != VK_SUCCESS) {
        free(vert_code);
        free(frag_code);
        enif_release_resource(res);
        return enif_make_badarg(env);
    }

    free(vert_code);
    free(frag_code);

    res->vert_shader = shaders_out[0];
    res->frag_shader = shaders_out[1];

    if (ubo_size > 0) {
        const VkBufferUsageFlags ubo_usage = VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT;
        const VkMemoryPropertyFlags ubo_mem =
            VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT |
            VK_MEMORY_PROPERTY_HOST_COHERENT_BIT;

        for (uint32_t i = 0; i < FRAMES_IN_FLIGHT; i++) {
            if (!create_gpu_buffer(&res->material_ubo[i],
                                   (VkDeviceSize)ubo_size, ubo_usage,
                                   ubo_mem)) {
                cleanup_partial_renderer(res);
                enif_release_resource(res);
                return enif_make_badarg(env);
            }

            uint32_t slot = alloc_material_index();
            if (slot == UINT32_MAX) {
                cleanup_partial_renderer(res);
                enif_release_resource(res);
                return enif_make_badarg(env);
            }

            res->material_index[i] = slot;

            VkDescriptorBufferInfo bi = {
                .buffer = res->material_ubo[i].buffer,
                .offset = 0,
                .range = (VkDeviceSize)ubo_size,
            };

            VkWriteDescriptorSet w = {
                .sType = VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET,
                .dstSet = g_device.descriptor_set,
                .dstBinding = UNIFORM_BINDING,
                .dstArrayElement = slot,
                .descriptorCount = 1,
                .descriptorType = VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,
                .pBufferInfo = &bi,
            };

            vkUpdateDescriptorSets(g_device.logical_device, 1, &w, 0, NULL);
        }
    }

    ERL_NIF_TERM handle_term = enif_make_resource(env, res);

    enif_release_resource(res);

    return handle_term;
}

uint32_t get_material_index_for_frame(const renderer_res_t* r, uint32_t frame) {
    if (!r) return UINT32_MAX;
    if (frame >= FRAMES_IN_FLIGHT) return UINT32_MAX;
    return r->material_index[frame];
}

static size_t align_up(size_t x, size_t a) { return (x + (a - 1)) & ~(a - 1); }

static int get_bool_term(ErlNifEnv* env, ERL_NIF_TERM t, uint32_t* out01) {
    ERL_NIF_TERM a_true = enif_make_atom(env, "true");
    ERL_NIF_TERM a_false = enif_make_atom(env, "false");
    if (enif_is_identical(t, a_true)) {
        *out01 = 1;
        return 1;
    }
    if (enif_is_identical(t, a_false)) {
        *out01 = 0;
        return 1;
    }
    return 0;
}

static int get_f32_term(ErlNifEnv* env, ERL_NIF_TERM t, float* out) {
    double d = 0.0;
    if (!enif_get_double(env, t, &d)) return 0;
    *out = (float)d;
    return 1;
}

static int get_u32_term(ErlNifEnv* env, ERL_NIF_TERM t, uint32_t* out) {
    unsigned long u = 0;
    if (!enif_get_ulong(env, t, &u)) return 0;
    if (u > UINT32_MAX) return 0;
    *out = (uint32_t)u;
    return 1;
}

static int write_bytes(uint8_t* dst, size_t cap, size_t off, const void* src,
                       size_t n) {
    if (off + n > cap) return 0;
    memcpy(dst + off, src, n);
    return 1;
}

static int write_zeros(uint8_t* dst, size_t cap, size_t off, size_t n) {
    if (off + n > cap) return 0;
    memset(dst + off, 0, n);
    return 1;
}

static int pack_std140_term(ErlNifEnv* env, ERL_NIF_TERM t, uint8_t* blob,
                            size_t blob_size, size_t* off) {
    uint32_t b01 = 0;
    if (get_bool_term(env, t, &b01)) {
        *off = align_up(*off, 4);
        return write_bytes(blob, blob_size, *off, &b01, 4) ? (*off += 4, 1) : 0;
    }

    float f = 0.0f;
    if (get_f32_term(env, t, &f)) {
        *off = align_up(*off, 4);
        return write_bytes(blob, blob_size, *off, &f, 4) ? (*off += 4, 1) : 0;
    }

    uint32_t u = 0;
    if (get_u32_term(env, t, &u)) {
        *off = align_up(*off, 4);
        return write_bytes(blob, blob_size, *off, &u, 4) ? (*off += 4, 1) : 0;
    }

    texture_res_t* tex = get_texture_from_term(env, t);

    if (tex) {
        uint32_t idx = tex->texture_index;
        *off = align_up(*off, 4);
        return write_bytes(blob, blob_size, *off, &idx, 4) ? (*off += 4, 1) : 0;
    }

    const ERL_NIF_TERM* elems = NULL;
    int arity = 0;
    if (!enif_get_tuple(env, t, &arity, &elems)) return 0;

    if (arity == 2) {
        *off = align_up(*off, 8);
        float v[2];
        if (!get_f32_term(env, elems[0], &v[0])) return 0;
        if (!get_f32_term(env, elems[1], &v[1])) return 0;
        if (!write_bytes(blob, blob_size, *off, v, sizeof(v))) return 0;
        *off += sizeof(v);
        return 1;
    }

    if (arity == 3) {
        *off = align_up(*off, 16);
        float v3[3];
        if (!get_f32_term(env, elems[0], &v3[0])) return 0;
        if (!get_f32_term(env, elems[1], &v3[1])) return 0;
        if (!get_f32_term(env, elems[2], &v3[2])) return 0;

        if (!write_bytes(blob, blob_size, *off, v3, 12)) return 0;
        if (!write_zeros(blob, blob_size, *off + 12, 4)) return 0;
        *off += 16;
        return 1;
    }

    if (arity == 9) {
        *off = align_up(*off, 16);

        float m[9];
        for (int i = 0; i < 9; i++) {
            if (!get_f32_term(env, elems[i], &m[i])) return 0;
        }

        for (int col = 0; col < 3; col++) {
            *off = align_up(*off, 16);
            float colv[3] = {
                m[col * 3 + 0],
                m[col * 3 + 1],
                m[col * 3 + 2],
            };
            if (!write_bytes(blob, blob_size, *off, colv, 12)) return 0;
            if (!write_zeros(blob, blob_size, *off + 12, 4)) return 0;
            *off += 16;
        }
        return 1;
    }

    if (arity == 16) {
        *off = align_up(*off, 16);

        float m[16];
        for (int i = 0; i < 16; i++) {
            if (!get_f32_term(env, elems[i], &m[i])) return 0;
        }

        for (int col = 0; col < 4; col++) {
            *off = align_up(*off, 16);
            float colv[4] = {
                m[col * 4 + 0],
                m[col * 4 + 1],
                m[col * 4 + 2],
                m[col * 4 + 3],
            };
            if (!write_bytes(blob, blob_size, *off, colv, 16)) return 0;
            *off += 16;
        }
        return 1;
    }

    return 0;
}

static int pack_std140_params_tuple(ErlNifEnv* env, ERL_NIF_TERM params_tuple,
                                    uint8_t* blob, size_t blob_size) {
    const ERL_NIF_TERM* elems = NULL;
    int arity = 0;
    if (!enif_get_tuple(env, params_tuple, &arity, &elems)) return 0;

    size_t off = 0;
    memset(blob, 0, blob_size);

    for (int i = 0; i < arity; i++) {
        if (!pack_std140_term(env, elems[i], blob, blob_size, &off)) return 0;
    }

    if (off > blob_size) return 0;

    return 1;
}

void update_material_for_frame(ErlNifEnv* env, const renderer_res_t* r,
                               uint32_t frame, ERL_NIF_TERM params) {
    if (!env || !r) return;
    if (frame >= FRAMES_IN_FLIGHT) return;

    GpuBuffer* buf = (GpuBuffer*)&r->material_ubo[frame];
    if (!buf || !buf->buffer || buf->size == 0) return;

    const size_t ubo_size = (size_t)buf->size;

    uint8_t* ubo_blob = (uint8_t*)enif_alloc(ubo_size);
    if (!ubo_blob) return;

    if (!pack_std140_params_tuple(env, params, ubo_blob, ubo_size)) {
        enif_fprintf(stderr,
                     "renderer: std140 pack failed or size mismatch "
                     "(frame=%u, ubo_size=%llu)\n",
                     (unsigned)frame, (unsigned long long)ubo_size);
        enif_free(ubo_blob);
        return;
    }

    if (!direct_write_gpu_buffer(buf, ubo_blob, (VkDeviceSize)ubo_size, 0)) {
        enif_fprintf(stderr,
                     "renderer: direct_write_gpu_buffer failed "
                     "(frame=%u, size=%zu)\n",
                     (unsigned)frame, ubo_size);
        enif_free(ubo_blob);
        return;
    }

    enif_free(ubo_blob);
}
