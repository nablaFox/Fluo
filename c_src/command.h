#pragma once

#include <vulkan/vulkan.h>

#include "erl_nif.h"

#define FRAMES_IN_FLIGHT 2

typedef struct {
    VkFence fences[FRAMES_IN_FLIGHT];
    VkCommandBuffer cmds[FRAMES_IN_FLIGHT];
    VkSemaphore finished_sem[FRAMES_IN_FLIGHT];
    uint32_t frame;
} command_res_t;

int nif_init_command_res(ErlNifEnv* env);

void destroy_command_pool();

command_res_t* get_command_from_term(ErlNifEnv* env, ERL_NIF_TERM term);

ERL_NIF_TERM nif_create_command(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

ERL_NIF_TERM nif_start_command_recording(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

ERL_NIF_TERM nif_end_command_recording(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

ERL_NIF_TERM nif_submit_command(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

ERL_NIF_TERM nif_start_rendering(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

ERL_NIF_TERM nif_end_rendering(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

ERL_NIF_TERM nif_draw_mesh(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
