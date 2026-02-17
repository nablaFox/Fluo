#include "params.h"

#include "texture.h"

static size_t align_up(size_t x, size_t a) { return (x + (a - 1)) & ~(a - 1); }

static int get_bool_term(ErlNifEnv* env, ERL_NIF_TERM t, uint32_t* out01) {
    ERL_NIF_TERM a_true = enif_make_atom(env, "true");
    ERL_NIF_TERM a_false = enif_make_atom(env, "false");
    if (enif_is_identical(t, a_true)) {
        *out01 = 1;
        return 1;
    }
    if (enif_is_identical(t, a_false)) {
        *out01 = 0;
        return 1;
    }
    return 0;
}

static int get_i32_term(ErlNifEnv* env, ERL_NIF_TERM t, int32_t* out) {
    int i = 0;
    if (enif_get_int(env, t, &i)) {
        *out = (int32_t)i;
        return 1;
    }
    return 0;
}

static int get_f32_term(ErlNifEnv* env, ERL_NIF_TERM t, float* out) {
    double d = 0.0;
    if (!enif_get_double(env, t, &d)) return 0;
    *out = (float)d;
    return 1;
}

static int get_u32_term(ErlNifEnv* env, ERL_NIF_TERM t, uint32_t* out) {
    unsigned long u = 0;
    if (!enif_get_ulong(env, t, &u)) return 0;
    if (u > UINT32_MAX) return 0;
    *out = (uint32_t)u;
    return 1;
}

static int write_bytes(uint8_t* dst, size_t cap, size_t off, const void* src,
                       size_t n) {
    if (off + n > cap) return 0;
    memcpy(dst + off, src, n);
    return 1;
}

static int write_zeros(uint8_t* dst, size_t cap, size_t off, size_t n) {
    if (off + n > cap) return 0;
    memset(dst + off, 0, n);
    return 1;
}

static int read_f32_list(ErlNifEnv* env, ERL_NIF_TERM list, float* out,
                         int expected_len) {
    ERL_NIF_TERM head, tail;
    ERL_NIF_TERM cur = list;

    for (int i = 0; i < expected_len; i++) {
        if (!enif_get_list_cell(env, cur, &head, &tail)) return 0;
        if (!get_f32_term(env, head, &out[i])) return 0;
        cur = tail;
    }

    if (!enif_is_empty_list(env, cur)) return 0;
    return 1;
}

static int is_tagged_tuple(ErlNifEnv* env, int arity,
                           const ERL_NIF_TERM* elems) {
    return (arity >= 2 && enif_is_atom(env, elems[0]));
}

static int pack_u32(uint8_t* blob, size_t blob_size, size_t* off, uint32_t v) {
    *off = align_up(*off, 4);
    if (!write_bytes(blob, blob_size, *off, &v, 4)) return 0;
    *off += 4;
    return 1;
}

static int pack_f32(uint8_t* blob, size_t blob_size, size_t* off, float v) {
    *off = align_up(*off, 4);
    if (!write_bytes(blob, blob_size, *off, &v, 4)) return 0;
    *off += 4;
    return 1;
}

static int pack_vec2_f32(uint8_t* blob, size_t blob_size, size_t* off,
                         const float v[2]) {
    *off = align_up(*off, 8);
    if (!write_bytes(blob, blob_size, *off, v, 8)) return 0;
    *off += 8;
    return 1;
}

static int pack_vec3_f32_std140(uint8_t* blob, size_t blob_size, size_t* off,
                                const float v[3]) {
    *off = align_up(*off, 16);
    if (!write_bytes(blob, blob_size, *off, v, 12)) return 0;
    if (!write_zeros(blob, blob_size, *off + 12, 4)) return 0;
    *off += 12;
    return 1;
}

static int pack_vec4_f32(uint8_t* blob, size_t blob_size, size_t* off,
                         const float v[4]) {
    *off = align_up(*off, 16);
    if (!write_bytes(blob, blob_size, *off, v, 16)) return 0;
    *off += 16;
    return 1;
}

static int pack_mat3_f32_std140(uint8_t* blob, size_t blob_size, size_t* off,
                                const float m9[9]) {
    *off = align_up(*off, 16);
    for (int col = 0; col < 3; col++) {
        float colv[3] = {
            m9[0 * 3 + col],
            m9[1 * 3 + col],
            m9[2 * 3 + col],
        };
        if (!pack_vec3_f32_std140(blob, blob_size, off, colv)) return 0;
    }
    return 1;
}

static int pack_mat4_f32_std140(uint8_t* blob, size_t blob_size, size_t* off,
                                const float m16[16]) {
    *off = align_up(*off, 16);
    for (int col = 0; col < 4; col++) {
        float colv[4] = {
            m16[0 * 4 + col],
            m16[1 * 4 + col],
            m16[2 * 4 + col],
            m16[3 * 4 + col],
        };
        if (!pack_vec4_f32(blob, blob_size, off, colv)) return 0;
    }
    return 1;
}

static int pack_tuple(ErlNifEnv* env, const ERL_NIF_TERM* elems, int base,
                      int n, uint8_t* blob, size_t blob_size, size_t* off) {
    if (n == 2) {
        float v[2];
        if (get_f32_term(env, elems[base + 0], &v[0]) &&
            get_f32_term(env, elems[base + 1], &v[1])) {
            return pack_vec2_f32(blob, blob_size, off, v) ? 1 : -1;
        }
        return 0;
    }

    if (n == 3) {
        float v[3];
        if (get_f32_term(env, elems[base + 0], &v[0]) &&
            get_f32_term(env, elems[base + 1], &v[1]) &&
            get_f32_term(env, elems[base + 2], &v[2])) {
            return pack_vec3_f32_std140(blob, blob_size, off, v) ? 1 : -1;
        }
        return 0;
    }

    if (n == 4) {
        float v[4];
        if (get_f32_term(env, elems[base + 0], &v[0]) &&
            get_f32_term(env, elems[base + 1], &v[1]) &&
            get_f32_term(env, elems[base + 2], &v[2]) &&
            get_f32_term(env, elems[base + 3], &v[3])) {
            return pack_vec4_f32(blob, blob_size, off, v) ? 1 : -1;
        }
        return 0;
    }

    if (n == 9) {
        float m[9];
        for (int i = 0; i < 9; i++) {
            if (!get_f32_term(env, elems[base + i], &m[i])) return 0;
        }
        return pack_mat3_f32_std140(blob, blob_size, off, m) ? 1 : -1;
    }

    if (n == 16) {
        float m[16];
        for (int i = 0; i < 16; i++) {
            if (!get_f32_term(env, elems[base + i], &m[i])) return 0;
        }
        return pack_mat4_f32_std140(blob, blob_size, off, m) ? 1 : -1;
    }

    return 0;
}

static int pack_std140_term(ErlNifEnv* env, ERL_NIF_TERM t, uint8_t* blob,
                            size_t blob_size, size_t* off) {
    uint32_t b01 = 0;
    if (get_bool_term(env, t, &b01)) {
        return pack_u32(blob, blob_size, off, b01);
    }

    float f = 0.0f;
    if (get_f32_term(env, t, &f)) {
        return pack_f32(blob, blob_size, off, f);
    }

    uint32_t u = 0;
    if (get_u32_term(env, t, &u)) {
        return pack_u32(blob, blob_size, off, u);
    }

    texture_res_t* tex = get_texture_from_term(env, t);
    if (tex) {
        return pack_u32(blob, blob_size, off, tex->texture_index);
    }

    if (enif_is_list(env, t)) {
        float v2[2];
        if (read_f32_list(env, t, v2, 2)) {
            return pack_vec2_f32(blob, blob_size, off, v2);
        }

        float v3[3];
        if (read_f32_list(env, t, v3, 3)) {
            return pack_vec3_f32_std140(blob, blob_size, off, v3);
        }

        float v4[4];
        if (read_f32_list(env, t, v4, 4)) {
            return pack_vec4_f32(blob, blob_size, off, v4);
        }

        float m3[9];
        if (read_f32_list(env, t, m3, 9)) {
            return pack_mat3_f32_std140(blob, blob_size, off, m3);
        }

        float m4[16];
        if (read_f32_list(env, t, m4, 16)) {
            return pack_mat4_f32_std140(blob, blob_size, off, m4);
        }

        return 0;
    }

    const ERL_NIF_TERM* elems = NULL;
    int arity = 0;
    if (!enif_get_tuple(env, t, &arity, &elems)) return 0;

    int tagged = is_tagged_tuple(env, arity, elems);

    if (tagged) {
        int n = arity - 1;
        int math_result = pack_tuple(env, elems, 1, n, blob, blob_size, off);
        if (math_result == 1) return 1;
        if (math_result == -1) return 0;

        for (int i = 1; i < arity; i++) {
            if (!pack_std140_term(env, elems[i], blob, blob_size, off))
                return 0;
        }
        return 1;
    }

    int math_result = pack_tuple(env, elems, 0, arity, blob, blob_size, off);

    if (math_result == 1) return 1;
    if (math_result == -1) return 0;

    for (int i = 0; i < arity; i++) {
        if (!pack_std140_term(env, elems[i], blob, blob_size, off)) return 0;
    }

    return 1;

    return 1;
}

int pack_std140_params_term(ErlNifEnv* env, ERL_NIF_TERM params_term,
                            uint8_t* blob, size_t blob_size) {
    if (enif_is_identical(params_term, enif_make_atom(env, "nil"))) {
        memset(blob, 0, blob_size);
        return 1;
    }

    size_t off = 0;
    memset(blob, 0, blob_size);

    if (!pack_std140_term(env, params_term, blob, blob_size, &off)) {
        enif_fprintf(stderr, "renderer: std140 pack failed (size=%llu)\n",
                     (unsigned long long)blob_size);
        return 0;
    }

    return off <= blob_size;
}

int get_viewport_from_term(ErlNifEnv* env, ERL_NIF_TERM term,
                           VkViewport* out_vp) {
    if (!out_vp) return 0;

    const ERL_NIF_TERM* elems = NULL;
    int arity = 0;

    if (!enif_get_tuple(env, term, &arity, &elems) || arity != 4) return 0;

    int32_t x, y;
    uint32_t w, h;

    if (!get_i32_term(env, elems[0], &x)) return 0;
    if (!get_i32_term(env, elems[1], &y)) return 0;
    if (!get_u32_term(env, elems[2], &w)) return 0;
    if (!get_u32_term(env, elems[3], &h)) return 0;

    *out_vp = (VkViewport){
        .x = (float)x,
        .y = (float)y,
        .width = (float)w,
        .height = (float)h,
        .minDepth = 0.0f,
        .maxDepth = 1.0f,
    };

    return 1;
}

int get_scissor_from_term(ErlNifEnv* env, ERL_NIF_TERM term,
                          VkRect2D* out_scissor) {
    if (!out_scissor) return 0;

    const ERL_NIF_TERM* elems = NULL;
    int arity = 0;

    if (!enif_get_tuple(env, term, &arity, &elems) || arity != 4) return 0;

    int32_t x, y;
    uint32_t w, h;

    if (!get_i32_term(env, elems[0], &x)) return 0;
    if (!get_i32_term(env, elems[1], &y)) return 0;
    if (!get_u32_term(env, elems[2], &w)) return 0;
    if (!get_u32_term(env, elems[3], &h)) return 0;

    *out_scissor = (VkRect2D){
        .offset = {x, y},
        .extent = {w, h},
    };

    return 1;
}
