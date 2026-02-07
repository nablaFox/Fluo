#include "device.h"
#include "erl_nif.h"
#include "window.h"

typedef struct {
    struct Window* win;
} window_res_t;

static ErlNifResourceType* WINDOW_RES_TYPE = NULL;

static void window_res_dtor(ErlNifEnv* env, void* obj) {
    (void)env;
    window_res_t* r = (window_res_t*)obj;
    if (r->win) {
        destroy_window(r->win);
        r->win = NULL;
    }
}

static int load(ErlNifEnv* env, void** priv, ERL_NIF_TERM info) {
    (void)env;
    (void)priv;
    (void)info;

    init_device();

    WINDOW_RES_TYPE =
        enif_open_resource_type(env, "fluo_nif", "window_res", window_res_dtor,
                                ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL);

    if (WINDOW_RES_TYPE == NULL) return -1;

    return 0;
}

static void unload(ErlNifEnv* env, void* priv) {
    (void)env;

    destroy_device();
}

static ERL_NIF_TERM nif_create_window(ErlNifEnv* env, int argc,
                                      const ERL_NIF_TERM argv[]) {
    if (argc != 3) return enif_make_badarg(env);

    int width, height;
    if (!enif_get_int(env, argv[1], &width)) return enif_make_badarg(env);
    if (!enif_get_int(env, argv[2], &height)) return enif_make_badarg(env);

    ErlNifBinary title_bin;
    if (!enif_inspect_binary(env, argv[0], &title_bin))
        return enif_make_badarg(env);

    char* title = strndup((char*)title_bin.data, title_bin.size);
    if (!title) return enif_make_badarg(env);

    struct Window* win = create_window(title, (size_t)width, (size_t)height);
    free(title);

    if (!win) return enif_make_badarg(env);

    window_res_t* res =
        enif_alloc_resource(WINDOW_RES_TYPE, sizeof(window_res_t));
    res->win = win;

    ERL_NIF_TERM handle_term = enif_make_resource(env, res);

    enif_release_resource(res);

    ERL_NIF_TERM width_term = enif_make_int(env, width);
    ERL_NIF_TERM height_term = enif_make_int(env, height);
    ERL_NIF_TERM title_term = argv[0];

    ERL_NIF_TERM color_term = enif_make_tuple2(env, enif_make_int(env, width),
                                               enif_make_int(env, height));

    ERL_NIF_TERM depth_term = enif_make_tuple2(env, enif_make_int(env, width),
                                               enif_make_int(env, height));

    return enif_make_tuple6(env, width_term, height_term, title_term,
                            color_term, depth_term, handle_term);
}

static ERL_NIF_TERM nif_window_should_close(ErlNifEnv* env, int argc,
                                            const ERL_NIF_TERM argv[]) {
    if (argc != 1) return enif_make_badarg(env);

    const ERL_NIF_TERM* elems = NULL;

    int arity = 0;

    if (!enif_get_tuple(env, argv[0], &arity, &elems))
        return enif_make_badarg(env);
    if (arity != 6) return enif_make_badarg(env);

    ERL_NIF_TERM handle_term = elems[5];

    window_res_t* res = NULL;
    if (!enif_get_resource(env, handle_term, WINDOW_RES_TYPE, (void**)&res))
        return enif_make_badarg(env);

    if (!res || !res->win) return enif_make_badarg(env);

    int should_close = window_should_close(res->win) ? 1 : 0;

    return should_close ? enif_make_atom(env, "true")
                        : enif_make_atom(env, "false");
}

static ErlNifFunc nif_funcs[] = {
    {"create_window", 3, nif_create_window},
    {"window_should_close", 1, nif_window_should_close},
};

ERL_NIF_INIT(fluo_nif, nif_funcs, load, NULL, NULL, NULL)
