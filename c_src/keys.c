#include "window.h"

ERL_NIF_TERM nif_window_keys_down(ErlNifEnv* env, int argc,
                                  const ERL_NIF_TERM argv[]) {
    window_res_t* w = get_window_from_term(env, argv[0]);

    if (!w || !w->handle) return enif_make_badarg(env);

    typedef struct {
        int glfw_key;
        const char* atom;
    } KeySpec;

    static const KeySpec ORDER[] = {
        {GLFW_KEY_ENTER, "enter"},
        {GLFW_KEY_ESCAPE, "escape"},
        {GLFW_KEY_SPACE, "space"},
        {GLFW_KEY_BACKSPACE, "backspace"},
        {GLFW_KEY_TAB, "tab"},

        {GLFW_KEY_LEFT_SHIFT, "l_shift"},
        {GLFW_KEY_RIGHT_SHIFT, "r_shift"},
        {GLFW_KEY_LEFT_CONTROL, "l_ctrl"},
        {GLFW_KEY_RIGHT_CONTROL, "r_ctrl"},
        {GLFW_KEY_LEFT_ALT, "l_alt"},
        {GLFW_KEY_RIGHT_ALT, "r_alt"},

        {GLFW_KEY_A, "a"},
        {GLFW_KEY_B, "b"},
        {GLFW_KEY_C, "c"},
        {GLFW_KEY_D, "d"},
        {GLFW_KEY_E, "e"},
        {GLFW_KEY_F, "f"},
        {GLFW_KEY_G, "g"},
        {GLFW_KEY_H, "h"},
        {GLFW_KEY_I, "i"},
        {GLFW_KEY_J, "j"},
        {GLFW_KEY_K, "k"},
        {GLFW_KEY_L, "l"},
        {GLFW_KEY_M, "m"},
        {GLFW_KEY_N, "n"},
        {GLFW_KEY_O, "o"},
        {GLFW_KEY_P, "p"},
        {GLFW_KEY_Q, "q"},
        {GLFW_KEY_R, "r"},
        {GLFW_KEY_S, "s"},
        {GLFW_KEY_T, "t"},
        {GLFW_KEY_U, "u"},
        {GLFW_KEY_V, "v"},
        {GLFW_KEY_W, "w"},
        {GLFW_KEY_X, "x"},
        {GLFW_KEY_Y, "y"},
        {GLFW_KEY_Z, "z"},

        {GLFW_KEY_UP, "up"},
        {GLFW_KEY_DOWN, "down"},
        {GLFW_KEY_LEFT, "left"},
        {GLFW_KEY_RIGHT, "right"},
    };

    ERL_NIF_TERM list = enif_make_list(env, 0);

    for (int i = (int)(sizeof(ORDER) / sizeof(ORDER[0])) - 1; i >= 0; --i) {
        if (glfwGetKey(w->handle, ORDER[i].glfw_key) == GLFW_PRESS) {
            list = enif_make_list_cell(env, enif_make_atom(env, ORDER[i].atom),
                                       list);
        }
    }

    return list;
}
