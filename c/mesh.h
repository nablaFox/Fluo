#pragma once

#include "erl_nif.h"

typedef struct {
    float px, py, pz;
    float cr, cg, cb;
} VertexGPU;

ERL_NIF_TERM nif_create_mesh(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

int nif_init_mesh_res(ErlNifEnv* env);
