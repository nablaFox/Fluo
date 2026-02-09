#pragma once

#include "buffer.h"
#include "rendering.h"

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
    uint32_t material_index[FRAMES_IN_FLIGHT];
    GpuBuffer material_ubo[FRAMES_IN_FLIGHT];
} renderer_res_t;

int nif_init_renderer_res(ErlNifEnv* env);

ERL_NIF_TERM nif_create_renderer(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

renderer_res_t* get_renderer_from_term(ErlNifEnv* env, ERL_NIF_TERM term);

uint32_t get_material_index_for_frame(const renderer_res_t* r, uint32_t frame);

void update_material_for_frame(ErlNifEnv* env, const renderer_res_t* r, uint32_t frame, ERL_NIF_TERM params);
