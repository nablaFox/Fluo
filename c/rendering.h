#pragma once

#include <erl_nif.h>

#define FRAMES_IN_FLIGHT 2

int init_rendering_res();

void destroy_rendering_res();

ERL_NIF_TERM nif_start_rendering(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

ERL_NIF_TERM nif_end_rendering(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

ERL_NIF_TERM nif_swap_buffers(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

ERL_NIF_TERM nif_draw_mesh(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
