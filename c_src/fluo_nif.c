#include "command.h"
#include "device.h"
#include "mesh.h"
#include "renderer.h"
#include "shaders.h"
#include "texture.h"
#include "window.h"

ErlNifMutex* g_vk_mutex = NULL;

static int load(ErlNifEnv* env, void** priv, ERL_NIF_TERM info) {
    (void)env;
    (void)priv;
    (void)info;

    g_vk_mutex = enif_mutex_create("vk-global");
    if (!g_vk_mutex) return -1;

    enif_mutex_lock(g_vk_mutex);

    // TODO: make it return an int for error handling
    init_device();

    if (nif_init_image_res(env) < 0) {
        return -1;
    }

    if (nif_init_window_res(env) < 0) {
        return -1;
    }

    if (nif_init_mesh_res(env) < 0) {
        return -1;
    }

    if (nif_init_renderer_res(env) < 0) {
        return -1;
    }

    if (nif_init_command_res(env) < 0) {
        return -1;
    }

    if (nif_init_texture_res(env) < 0) {
        return -1;
    }

    enif_mutex_unlock(g_vk_mutex);

    return 0;
}

static void unload(ErlNifEnv* env, void* priv) {
    (void)env;

    destroy_command_pool();

    destroy_device();
}

static ErlNifFunc nif_funcs[] = {
    {"create_window", 3, nif_create_window},
    {"window_should_close", 1, nif_window_should_close},
    {"window_keys_down", 1, nif_window_keys_down},
    {"window_mouse_pos", 1, nif_window_mouse_pos},
    {"window_mouse_delta", 1, nif_window_mouse_delta},
    {"window_delta_time", 1, nif_window_delta_time},
    {"window_capture_mouse", 1, nif_window_capture_mouse},
    {"window_mouse_left_down", 1, nif_window_mouse_left_down},
    {"window_mouse_right_down", 1, nif_window_mouse_right_down},
    {"window_release_mouse", 1, nif_window_release_mouse},
    {"create_mesh_allocator", 2, nif_create_mesh_allocator},
    {"allocate_mesh", 3, nif_allocate_mesh},
    {"write_mesh", 3, nif_write_mesh},
    {"submit_mesh_writes", 1, nif_submit_mesh_writes},
    {"create_renderer", 3, nif_create_renderer},
    {"create_depth_image", 2, nif_create_depth_image},
    {"create_color_image", 2, nif_create_color_image},
    {"read_image", 1, nif_read_image},
    {"create_texture", 3, nif_create_texture},
    {"load_texture_from_path", 1, nif_load_texture_from_path},
    {"save_color_image", 2, nif_save_color_image_to_png},
    {"swap_buffers", 3, nif_swap_buffers},
    {"set_frame_params", 3, nif_set_frame_params},
    {"create_command", 0, nif_create_command},
    {"start_command_recording", 1, nif_start_command_recording},
    {"end_command_recording", 1, nif_end_command_recording},
    {"submit_command", 1, nif_submit_command},
    {"start_rendering", 3, nif_start_rendering},
    {"end_rendering", 1, nif_end_rendering},
    {"draw_mesh", 6, nif_draw_mesh},
};

ERL_NIF_INIT(fluo_nif, nif_funcs, load, NULL, NULL, unload)
