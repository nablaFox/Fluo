#pragma once

#include <erl_nif.h>
#include <stdint.h>
#include <vulkan/vulkan.h>
#include "vk_mem_alloc.h"

typedef struct {
    float px, py, pz;
    float cr, cg, cb;
} VertexGPU;

typedef struct {
    uint32_t vertices_count;
    uint32_t indices_count;

    VkBuffer vertex_buf;
    VmaAllocation vertex_alloc;

    VkBuffer index_buf;
    VmaAllocation index_alloc;
} mesh_res_t;

ERL_NIF_TERM nif_create_mesh(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

int nif_init_mesh_res(ErlNifEnv* env);
