#include "renderer.h"

#include <assert.h>
#include <stdatomic.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "device.h"
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
        if (arity != 5) return NULL;

        if (!enif_is_atom(env, elems[0])) return NULL;
        if (!enif_is_identical(elems[0], enif_make_atom(env, "renderer")))
            return NULL;

        handle_term = elems[4];
    }

    renderer_res_t* res = NULL;

    if (!enif_get_resource(env, handle_term, RENDERER_RES_TYPE, (void**)&res))
        return NULL;

    return res;
}

typedef struct {
    param_kind_t kind;
    float f[16];
    int i;
} decoded_param_t;

static int get_atom(ErlNifEnv* env, ERL_NIF_TERM t, char* out, unsigned cap) {
    return enif_get_atom(env, t, out, cap, ERL_NIF_LATIN1);
}

static inline size_t align_up(size_t x, size_t a) {
    return (x + (a - 1)) & ~(a - 1);
}

static int decode_param(ErlNifEnv* env, ERL_NIF_TERM term,
                        decoded_param_t* out) {
    const ERL_NIF_TERM* tup = NULL;
    int arity = 0;
    if (!enif_get_tuple(env, term, &arity, &tup) || arity < 2) return 0;

    char tag[16];
    if (!get_atom(env, tup[0], tag, sizeof(tag))) return 0;

    if (strcmp(tag, "bool") == 0 && arity == 2) {
        int b;

        if (!enif_get_int(env, tup[1], &b)) {
            if (enif_is_identical(tup[1], enif_make_atom(env, "true")))
                b = 1;
            else if (enif_is_identical(tup[1], enif_make_atom(env, "false")))
                b = 0;
            else
                return 0;
        }

        out->kind = P_BOOL;
        out->i = b ? 1 : 0;
        return 1;
    }

    if (strcmp(tag, "f32") == 0 && arity == 2) {
        double d;
        if (!enif_get_double(env, tup[1], &d)) return 0;
        out->kind = P_F32;
        out->f[0] = (float)d;
        return 1;
    }

    if (strcmp(tag, "vec2") == 0 && arity == 3) {
        double a, b;
        if (!enif_get_double(env, tup[1], &a)) return 0;
        if (!enif_get_double(env, tup[2], &b)) return 0;
        out->kind = P_VEC2;
        out->f[0] = (float)a;
        out->f[1] = (float)b;
        return 1;
    }

    if (strcmp(tag, "vec3") == 0 && arity == 4) {
        double a, b, c;
        if (!enif_get_double(env, tup[1], &a)) return 0;
        if (!enif_get_double(env, tup[2], &b)) return 0;
        if (!enif_get_double(env, tup[3], &c)) return 0;
        out->kind = P_VEC3;
        out->f[0] = (float)a;
        out->f[1] = (float)b;
        out->f[2] = (float)c;
        return 1;
    }

    if (strcmp(tag, "vec4") == 0 && arity == 5) {
        double a, b, c, d;
        if (!enif_get_double(env, tup[1], &a)) return 0;
        if (!enif_get_double(env, tup[2], &b)) return 0;
        if (!enif_get_double(env, tup[3], &c)) return 0;
        if (!enif_get_double(env, tup[4], &d)) return 0;
        out->kind = P_VEC4;
        out->f[0] = (float)a;
        out->f[1] = (float)b;
        out->f[2] = (float)c;
        out->f[3] = (float)d;
        return 1;
    }

    if (strcmp(tag, "mat3") == 0 && arity == 10) {
        out->kind = P_MAT3;
        for (int i = 0; i < 9; i++) {
            double d;
            if (!enif_get_double(env, tup[1 + i], &d)) return 0;
            out->f[i] = (float)d;
        }
        return 1;
    }

    if (strcmp(tag, "mat4") == 0 && arity == 17) {
        out->kind = P_MAT4;
        for (int i = 0; i < 16; i++) {
            double d;
            if (!enif_get_double(env, tup[1 + i], &d)) return 0;
            out->f[i] = (float)d;
        }
        return 1;
    }

    return 0;
}

static void std140_info(param_kind_t k, size_t* align, size_t* size) {
    switch (k) {
        case P_BOOL:
            *align = 4;
            *size = 4;
            break;
        case P_F32:
            *align = 4;
            *size = 4;
            break;
        case P_VEC2:
            *align = 8;
            *size = 8;
            break;
        case P_VEC3:
            *align = 16;
            *size = 16;
            break;
        case P_VEC4:
            *align = 16;
            *size = 16;
            break;
        case P_MAT3:
            *align = 16;
            *size = 48;
            break;
        case P_MAT4:
            *align = 16;
            *size = 64;
            break;
        default:
            *align = 16;
            *size = 16;
            break;
    }
}

static int build_material_ubo_blob_std140(ErlNifEnv* env,
                                          ERL_NIF_TERM params_list,
                                          uint8_t** out_blob,
                                          size_t* out_size) {
    unsigned len = 0;
    if (!enif_get_list_length(env, params_list, &len)) return 0;

    size_t off = 0;
    ERL_NIF_TERM head, tail = params_list;

    for (unsigned i = 0; i < len; i++) {
        if (!enif_get_list_cell(env, tail, &head, &tail)) return 0;

        decoded_param_t p;
        if (!decode_param(env, head, &p)) return 0;

        size_t al, sz;
        std140_info(p.kind, &al, &sz);
        off = align_up(off, al);
        off += sz;
    }

    size_t total = align_up(off, 16);
    uint8_t* blob = (uint8_t*)enif_alloc(total);
    if (!blob) return 0;
    memset(blob, 0, total);

    off = 0;
    tail = params_list;
    for (unsigned i = 0; i < len; i++) {
        enif_get_list_cell(env, tail, &head, &tail);

        decoded_param_t p;
        decode_param(env, head, &p);

        size_t al, sz;
        std140_info(p.kind, &al, &sz);
        off = align_up(off, al);

        switch (p.kind) {
            case P_BOOL: {
                int32_t v = (int32_t)p.i;
                memcpy(blob + off, &v, 4);
            } break;
            case P_F32: {
                memcpy(blob + off, &p.f[0], 4);
            } break;
            case P_VEC2: {
                memcpy(blob + off, &p.f[0], 8);
            } break;
            case P_VEC3: {
                memcpy(blob + off, &p.f[0], 12);
            } break;
            case P_VEC4: {
                memcpy(blob + off, &p.f[0], 16);
            } break;
            case P_MAT3: {
                for (int col = 0; col < 3; col++) {
                    memcpy(blob + off + col * 16, &p.f[col * 3], 12);
                }
            } break;
            case P_MAT4: {
                memcpy(blob + off, &p.f[0], 64);
            } break;
            default:
                break;
        }

        off += sz;
    }

    *out_blob = blob;
    *out_size = total;
    return 1;
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
    if (argc != 3) return enif_make_badarg(env);

    char* vert_path = NULL;
    char* frag_path = NULL;

    if (!get_c_string_from_gleam_string(env, argv[1], &vert_path))
        return enif_make_badarg(env);

    if (!get_c_string_from_gleam_string(env, argv[2], &frag_path)) {
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

    VkResult vr = vkCreateShadersEXT_(g_device.logical_device, 2, shader_infos,
                                      NULL, shaders_out);

    free(vert_code);
    free(frag_code);

    if (vr != VK_SUCCESS) {
        enif_release_resource(res);
        return enif_make_badarg(env);
    }

    res->vert_shader = shaders_out[0];
    res->frag_shader = shaders_out[1];

    uint8_t* ubo_blob = NULL;
    size_t ubo_size = 0;

    if (!build_material_ubo_blob_std140(env, argv[0], &ubo_blob, &ubo_size)) {
        cleanup_partial_renderer(res);
        enif_release_resource(res);
        return enif_make_badarg(env);
    }

    const VkBufferUsageFlags ubo_usage = VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT;
    const VkMemoryPropertyFlags ubo_mem = VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT |
                                          VK_MEMORY_PROPERTY_HOST_COHERENT_BIT;

    for (uint32_t i = 0; i < FRAMES_IN_FLIGHT; i++) {
        if (!create_gpu_buffer(&res->material_ubo[i], (VkDeviceSize)ubo_size,
                               ubo_usage, ubo_mem)) {
            enif_free(ubo_blob);
            cleanup_partial_renderer(res);
            enif_release_resource(res);
            return enif_make_badarg(env);
        }

        if (!direct_write_gpu_buffer(&res->material_ubo[i], ubo_blob,
                                     (VkDeviceSize)ubo_size, 0)) {
            enif_free(ubo_blob);
            cleanup_partial_renderer(res);
            enif_release_resource(res);
            return enif_make_badarg(env);
        }

        uint32_t slot = alloc_material_index();
        if (slot == UINT32_MAX) {
            enif_free(ubo_blob);
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

    enif_free(ubo_blob);
    ubo_blob = NULL;

    ERL_NIF_TERM handle_term = enif_make_resource(env, res);
    enif_release_resource(res);

    return enif_make_tuple3(env, argv[1], argv[2], handle_term);
}

uint32_t get_material_index_for_frame(const renderer_res_t* r, uint32_t frame) {
    if (!r) return UINT32_MAX;
    if (frame >= FRAMES_IN_FLIGHT) return UINT32_MAX;
    return r->material_index[frame];
}

void update_material_for_frame(ErlNifEnv* env, const renderer_res_t* r,
                               uint32_t frame, ERL_NIF_TERM params) {
    if (!env || !r) return;
    if (frame >= FRAMES_IN_FLIGHT) return;

    uint8_t* ubo_blob = NULL;
    size_t ubo_size = 0;

    if (!build_material_ubo_blob_std140(env, params, &ubo_blob, &ubo_size)) {
        enif_fprintf(stderr, "renderer: failed to decode material params\n");
        return;
    }

    GpuBuffer* buf = (GpuBuffer*)&r->material_ubo[frame];

    if (!direct_write_gpu_buffer(buf, ubo_blob, (VkDeviceSize)ubo_size, 0)) {
        enif_fprintf(
            stderr,
            "renderer: direct_write_gpu_buffer failed (frame=%u, size=%zu)\n",
            (unsigned)frame, ubo_size);
        enif_free(ubo_blob);
        return;
    }

    enif_free(ubo_blob);
}
