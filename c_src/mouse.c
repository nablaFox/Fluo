#include "window.h"

static inline void reset_mouse_tracking(window_res_t* w) {
    if (!w) return;

    w->accum_dx = 0.0;
    w->accum_dy = 0.0;
    w->has_last_cb = 0;
    w->last_cb_x = 0.0;
    w->last_cb_y = 0.0;
}

ERL_NIF_TERM nif_window_mouse_pos(ErlNifEnv* env, int argc,
                                  const ERL_NIF_TERM argv[]) {
    window_res_t* w = get_window_from_term(env, argv[0]);

    if (!w || !w->handle) return enif_make_badarg(env);

    if (glfwGetInputMode(w->handle, GLFW_CURSOR) == GLFW_CURSOR_DISABLED) {
        return enif_make_tuple3(env, enif_make_atom(env, "position"),
                                enif_make_double(env, 0.0),
                                enif_make_double(env, 0.0));
    }

    return enif_make_tuple3(env, enif_make_atom(env, "position"),
                            enif_make_double(env, w->curr_cb_x),
                            enif_make_double(env, w->curr_cb_y));
}

ERL_NIF_TERM nif_window_mouse_delta(ErlNifEnv* env, int argc,
                                    const ERL_NIF_TERM argv[]) {
    window_res_t* w = get_window_from_term(env, argv[0]);

    if (!w || !w->handle) return enif_make_badarg(env);

    if (!glfwGetWindowAttrib(w->handle, GLFW_FOCUSED)) {
        reset_mouse_tracking(w);
        return enif_make_tuple3(env, enif_make_atom(env, "position"),
                                enif_make_double(env, 0.0),
                                enif_make_double(env, 0.0));
    }

    double dx = w->accum_dx;
    double dy = w->accum_dy;

    w->accum_dx = 0.0;
    w->accum_dy = 0.0;

    if (dx > 80.0) dx = 80.0;
    if (dx < -80.0) dx = -80.0;
    if (dy > 80.0) dy = 80.0;
    if (dy < -80.0) dy = -80.0;

    return enif_make_tuple3(env, enif_make_atom(env, "position"),
                            enif_make_double(env, dx),
                            enif_make_double(env, dy));
}

ERL_NIF_TERM nif_window_capture_mouse(ErlNifEnv* env, int argc,
                                      const ERL_NIF_TERM argv[]) {
    window_res_t* w = get_window_from_term(env, argv[0]);

    if (!w || !w->handle) return enif_make_badarg(env);

    glfwSetInputMode(w->handle, GLFW_CURSOR, GLFW_CURSOR_DISABLED);
    reset_mouse_tracking(w);

    return enif_make_atom(env, "ok");
}

ERL_NIF_TERM nif_window_release_mouse(ErlNifEnv* env, int argc,
                                      const ERL_NIF_TERM argv[]) {
    window_res_t* w = get_window_from_term(env, argv[0]);

    if (!w || !w->handle) return enif_make_badarg(env);

    glfwSetInputMode(w->handle, GLFW_CURSOR, GLFW_CURSOR_NORMAL);
    reset_mouse_tracking(w);

    return enif_make_atom(env, "ok");
}
