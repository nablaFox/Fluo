#pragma once

#include <erl_nif.h>
#include <vulkan/vulkan.h>
#include "vk_mem_alloc.h"
#include "buffer.h"

typedef struct {
    uint32_t material_index;
} PushConstants;

typedef enum {
    P_BOOL,
    P_F32,
    P_VEC2,
    P_VEC3,
    P_VEC4,
    P_MAT3,
    P_MAT4,
} param_kind_t;

typedef struct {
    VkShaderEXT frag_shader;
    VkShaderEXT vert_shader;
    GpuBuffer material_ubo;
} renderer_res_t;

int nif_init_renderer_res(ErlNifEnv* env);

ERL_NIF_TERM nif_create_renderer(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
