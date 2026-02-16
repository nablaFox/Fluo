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
    GpuBuffer vertex_buffer;
    GpuBuffer index_buffer;
    GpuBuffer staging_buffer;

    VkDeviceSize vertex_heap;
    VkDeviceSize index_heap;
    VkDeviceSize vertex_buffer_size;
    VkDeviceSize index_buffer_size;
} mesh_allocator_res_t;

typedef struct {
    VkDeviceSize vertex_offset;
    VkDeviceSize vertex_size;

    VkDeviceSize index_offset;
    VkDeviceSize index_size;

    VkDeviceSize index_staging_offset;

    uint32_t indices_count;

    mesh_allocator_res_t* allocator;
} mesh_res_t;

int nif_init_mesh_res(ErlNifEnv* env);

ERL_NIF_TERM nif_create_mesh_allocator(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

ERL_NIF_TERM nif_allocate_mesh(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

ERL_NIF_TERM nif_write_mesh(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

ERL_NIF_TERM nif_submit_mesh_writes(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

mesh_res_t* get_mesh_from_term(ErlNifEnv* env, ERL_NIF_TERM term);

mesh_allocator_res_t* get_allocator_from_term(ErlNifEnv* env, ERL_NIF_TERM term);

#ifdef __cplusplus
}
#endif
