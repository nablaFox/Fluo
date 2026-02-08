#pragma once

#include <erl_nif.h>
#include <vulkan/vulkan.h>

#include "buffer.h"

typedef struct {
    uint32_t vertices_count;
    uint32_t indices_count;

    GpuBuffer vertex;
    GpuBuffer index;
} mesh_res_t;

int nif_init_mesh_res(ErlNifEnv* env);

ERL_NIF_TERM nif_create_mesh(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
