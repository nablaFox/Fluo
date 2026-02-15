#pragma once

#include "buffer.h"
#include "command.h"

typedef struct {
    uint32_t material_index;
    uint32_t frame_params_index;
    uint32_t padding[2];
    uint8_t draw_params[110];
} PushConstants;

typedef struct {
    VkShaderEXT frag_shader;
    VkShaderEXT vert_shader;

    GpuBuffer frame_params_ubo[FRAMES_IN_FLIGHT];
    uint32_t frame_params_index[FRAMES_IN_FLIGHT];

    GpuBuffer material_ubo;
    uint32_t material_index;
} renderer_res_t;

int nif_init_renderer_res(ErlNifEnv* env);

ERL_NIF_TERM nif_create_renderer(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

ERL_NIF_TERM nif_set_frame_params(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

renderer_res_t* get_renderer_from_term(ErlNifEnv* env, ERL_NIF_TERM term);

uint32_t get_frame_params_index(const renderer_res_t* r, uint32_t frame);
