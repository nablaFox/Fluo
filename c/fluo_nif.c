#include "device.h"
#include "mesh.h"
#include "renderer.h"
#include "window.h"

static int load(ErlNifEnv* env, void** priv, ERL_NIF_TERM info) {
    (void)env;
    (void)priv;
    (void)info;

    init_device();

    if (nif_init_window_res(env) < 0) {
        return -1;
    }

    if (nif_init_mesh_res(env) < 0) {
        return -1;
    }

    if (nif_init_renderer_res(env) < 0) {
        return -1;
    }

    return 0;
}

static void unload(ErlNifEnv* env, void* priv) {
    (void)env;

    destroy_device();
}

static ErlNifFunc nif_funcs[] = {
    {"create_window", 3, nif_create_window},
    {"window_should_close", 1, nif_window_should_close},
    {"create_mesh", 2, nif_create_mesh},
    {"create_renderer", 3, nif_create_renderer},
};

ERL_NIF_INIT(fluo_nif, nif_funcs, load, NULL, NULL, NULL)
