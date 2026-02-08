#pragma once

#include <erl_nif.h>

int get_c_string_from_gleam_string(ErlNifEnv* env, ERL_NIF_TERM t, char** out);

int is_tag(ErlNifEnv* env, ERL_NIF_TERM term, const char* a, const char* b);

int decode_f32(ErlNifEnv* env, ERL_NIF_TERM t, float* out);

int decode_vec3(ErlNifEnv* env, ERL_NIF_TERM t, float* x, float* y, float* z);

int decode_color(ErlNifEnv* env, ERL_NIF_TERM t, float* r, float* g, float* b);
