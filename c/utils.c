#include "utils.h"

int get_c_string_from_gleam_string(ErlNifEnv* env, ERL_NIF_TERM t, char** out) {
    ErlNifBinary bin;
    if (!enif_inspect_binary(env, t, &bin)) return 0;

    char* s = (char*)malloc(bin.size + 1);
    if (!s) return 0;

    memcpy(s, bin.data, bin.size);
    s[bin.size] = '\0';
    *out = s;
    return 1;
}

int is_tag(ErlNifEnv* env, ERL_NIF_TERM term, const char* a, const char* b) {
    char buf[32];
    if (!enif_get_atom(env, term, buf, sizeof(buf), ERL_NIF_LATIN1)) return 0;
    return (strcmp(buf, a) == 0) || (b && strcmp(buf, b) == 0);
}

int decode_f32(ErlNifEnv* env, ERL_NIF_TERM t, float* out) {
    double d;
    if (!enif_get_double(env, t, &d)) return 0;
    *out = (float)d;
    return 1;
}

int decode_vec3(ErlNifEnv* env, ERL_NIF_TERM t, float* x, float* y, float* z) {
    const ERL_NIF_TERM* e;
    int arity = 0;
    if (!enif_get_tuple(env, t, &arity, &e)) return 0;
    if (arity != 4) return 0;
    if (!is_tag(env, e[0], "vec3", "Vec3")) return 0;

    return decode_f32(env, e[1], x) && decode_f32(env, e[2], y) &&
           decode_f32(env, e[3], z);
}

int decode_color(ErlNifEnv* env, ERL_NIF_TERM t, float* r, float* g, float* b) {
    const ERL_NIF_TERM* e;
    int arity = 0;
    if (!enif_get_tuple(env, t, &arity, &e)) return 0;
    if (arity != 4) return 0;
    if (!is_tag(env, e[0], "color", "Color")) return 0;

    return decode_f32(env, e[1], r) && decode_f32(env, e[2], g) &&
           decode_f32(env, e[3], b);
}
