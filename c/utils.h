#pragma once

#include <erl_nif.h>
#include <vulkan/vulkan.h>

int get_c_string_from_gleam_string(ErlNifEnv* env, ERL_NIF_TERM t, char** out);

int is_tag(ErlNifEnv* env, ERL_NIF_TERM term, const char* a, const char* b);

int decode_f32(ErlNifEnv* env, ERL_NIF_TERM t, float* out);

static inline ERL_NIF_TERM raise_vk_error(ErlNifEnv* env, const char* expr, VkResult result) {
    ERL_NIF_TERM exc = enif_make_tuple3(env, enif_make_atom(env, "vulkan_error"), enif_make_atom(env, expr),
                                        enif_make_int(env, (int)result));

    return enif_raise_exception(env, exc);
}

#define THROW_VK_ERROR(env, expr)                         \
    do {                                                  \
        VkResult _vk_res = (expr);                        \
        if (_vk_res != VK_SUCCESS) {                      \
            return raise_vk_error((env), #expr, _vk_res); \
        }                                                 \
    } while (0)
