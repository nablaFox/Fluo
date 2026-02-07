#include "device.h"
#include "erl_nif.h"
#include "window.h"

static int load(ErlNifEnv* env, void** priv, ERL_NIF_TERM info) {
    (void)env;
    (void)priv;
    (void)info;

    init_device();

    return 0;
}

static void unload(ErlNifEnv* env, void* priv) {
    (void)env;

    destroy_device();
}

static ERL_NIF_TERM nif_create_window(ErlNifEnv* env, int argc,
                                      const ERL_NIF_TERM argv[]) {
    assert(argc == 3);

    int width, height;

    if (!enif_get_int(env, argv[1], &width)) return enif_make_badarg(env);
    if (!enif_get_int(env, argv[2], &height)) return enif_make_badarg(env);

    ErlNifBinary title_bin;

    if (!enif_inspect_binary(env, argv[0], &title_bin))
        return enif_make_badarg(env);

    char* title = strndup((char*)title_bin.data, title_bin.size);

    struct Window* win = create_window(title, (size_t)width, (size_t)height);

    free(title);

    ERL_NIF_TERM width_term = enif_make_int(env, width);
    ERL_NIF_TERM height_term = enif_make_int(env, height);
    ERL_NIF_TERM title_term = argv[2];

    ERL_NIF_TERM color_term = enif_make_tuple2(env, enif_make_int(env, width),
                                               enif_make_int(env, height));

    ERL_NIF_TERM depth_term = enif_make_tuple2(env, enif_make_int(env, width),
                                               enif_make_int(env, height));

    return enif_make_tuple5(env, width_term, height_term, title_term,
                            color_term, depth_term);
}

static ErlNifFunc nif_funcs[] = {
    {"create_window", 3, nif_create_window},
};

ERL_NIF_INIT(fluo_nif, nif_funcs, load, NULL, NULL, NULL)
