#pragma once

#include <erl_nif.h>
#include <vulkan/vulkan.h>

#include "buffer.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef struct {
    float pos[3];
    float normal[3];
    float uv[2];
} VertexGPU;

typedef struct {
    uint32_t vertices_count;
    uint32_t indices_count;

    GpuBuffer vertex_buffer;
    GpuBuffer index_buffer;
} mesh_res_t;

int nif_init_mesh_res(ErlNifEnv* env);

ERL_NIF_TERM nif_create_mesh(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

ERL_NIF_TERM nif_load_mesh_from_obj(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

mesh_res_t* get_mesh_from_term(ErlNifEnv* env, ERL_NIF_TERM term);

mesh_res_t* create_mesh(const VertexGPU* vertices, uint32_t vcount, const uint32_t* indices, uint32_t icount);

#ifdef __cplusplus
}
#endif
