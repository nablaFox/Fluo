#include "device.h"
#include "mesh.h"
#include "renderer.h"
#include "rendering.h"
#include "shaders.h"
#include "texture.h"
#include "window.h"

static int load(ErlNifEnv* env, void** priv, ERL_NIF_TERM info) {
    (void)env;
    (void)priv;
    (void)info;

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

    if (init_rendering_res() < 0) {
        return -1;
    }

    if (nif_init_texture_res(env) < 0) {
        return -1;
    }

    return 0;
}

static void unload(ErlNifEnv* env, void* priv) {
    (void)env;

    destroy_device();

    destroy_rendering_res();
}

static ErlNifFunc nif_funcs[] = {
    {"create_window", 3, nif_create_window},
    {"window_should_close", 1, nif_window_should_close},
    {"window_keys_down", 1, nif_window_keys_down},
    {"window_mouse_pos", 1, nif_window_mouse_pos},
    {"window_mouse_delta", 1, nif_window_mouse_delta},
    {"window_delta_time", 1, nif_window_delta_time},
    {"window_capture_mouse", 1, nif_window_capture_mouse},
    {"window_release_mouse", 1, nif_window_release_mouse},
    {"create_mesh", 2, nif_create_mesh},
    {"load_mesh_from_obj", 1, nif_load_mesh_from_obj},
    {"create_renderer", 3, nif_create_renderer},
    {"start_rendering", 2, nif_start_rendering},
    {"draw_mesh", 5, nif_draw_mesh},
    {"end_rendering", 0, nif_end_rendering},
    {"swap_buffers", 2, nif_swap_buffers},
    {"create_depth_image", 2, nif_create_depth_image},
    {"create_color_image", 2, nif_create_color_image},
    {"read_image", 1, nif_read_image},
    {"create_texture", 3, nif_create_texture},
    {"load_texture_from_path", 1, nif_load_texture_from_path},
    {"save_color_image", 2, nif_save_color_image_to_png},
};

ERL_NIF_INIT(fluo_nif, nif_funcs, load, NULL, NULL, unload)
