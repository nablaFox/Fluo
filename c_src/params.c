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

static int pack_std140_term(ErlNifEnv* env, ERL_NIF_TERM t, uint8_t* blob,
                            size_t blob_size, size_t* off) {
    uint32_t b01 = 0;
    if (get_bool_term(env, t, &b01)) {
        *off = align_up(*off, 4);
        if (!write_bytes(blob, blob_size, *off, &b01, 4)) return 0;
        *off += 4;
        return 1;
    }

    float f = 0.0f;
    if (get_f32_term(env, t, &f)) {
        *off = align_up(*off, 4);
        if (!write_bytes(blob, blob_size, *off, &f, 4)) return 0;
        *off += 4;
        return 1;
    }

    uint32_t u = 0;
    if (get_u32_term(env, t, &u)) {
        *off = align_up(*off, 4);
        if (!write_bytes(blob, blob_size, *off, &u, 4)) return 0;
        *off += 4;
        return 1;
    }

    texture_res_t* tex = get_texture_from_term(env, t);

    if (tex) {
        uint32_t idx = tex->texture_index;
        *off = align_up(*off, 4);
        if (!write_bytes(blob, blob_size, *off, &idx, 4)) return 0;
        *off += 4;
        return 1;
    }

    if (enif_is_list(env, t)) {
        float v2[2];
        if (read_f32_list(env, t, v2, 2)) {
            *off = align_up(*off, 8);
            if (!write_bytes(blob, blob_size, *off, v2, sizeof(v2))) return 0;
            *off += sizeof(v2);
            return 1;
        }

        float v3[3];
        if (read_f32_list(env, t, v3, 3)) {
            *off = align_up(*off, 16);
            if (!write_bytes(blob, blob_size, *off, v3, 12)) return 0;
            if (!write_zeros(blob, blob_size, *off + 12, 4)) return 0;
            *off += 16;
            return 1;
        }

        float m3[9];
        if (read_f32_list(env, t, m3, 9)) {
            *off = align_up(*off, 16);
            for (int col = 0; col < 3; col++) {
                *off = align_up(*off, 16);
                float colv[3] = {
                    m3[0 * 3 + col],  // row 0, col
                    m3[1 * 3 + col],  // row 1, col
                    m3[2 * 3 + col],  // row 2, col
                };
                if (!write_bytes(blob, blob_size, *off, colv, 12)) return 0;
                if (!write_zeros(blob, blob_size, *off + 12, 4)) return 0;
                *off += 16;
            }
            return 1;
        }

        float m4[16];
        if (read_f32_list(env, t, m4, 16)) {
            *off = align_up(*off, 16);
            for (int col = 0; col < 4; col++) {
                *off = align_up(*off, 16);
                float colv[4] = {
                    m4[0 * 4 + col],  // row 0, col
                    m4[1 * 4 + col],  // row 1, col
                    m4[2 * 4 + col],  // row 2, col
                    m4[3 * 4 + col],  // row 3, col
                };
                if (!write_bytes(blob, blob_size, *off, colv, 16)) return 0;
                *off += 16;
            }
            return 1;
        }

        return 0;
    }

    const ERL_NIF_TERM* elems = NULL;
    int arity = 0;
    if (!enif_get_tuple(env, t, &arity, &elems)) return 0;

    int tagged = 0;
    int base = 0;

    if (arity >= 2 && enif_is_atom(env, elems[0])) {
        tagged = 1;
        base = 1;
    }

    int n = arity - base;

    if (n == 2) {
        *off = align_up(*off, 8);
        float v[2];
        if (!get_f32_term(env, elems[base + 0], &v[0])) return 0;
        if (!get_f32_term(env, elems[base + 1], &v[1])) return 0;
        if (!write_bytes(blob, blob_size, *off, v, sizeof(v))) return 0;
        *off += sizeof(v);
        return 1;
    }

    if (n == 3) {
        *off = align_up(*off, 16);
        float v3[3];
        if (!get_f32_term(env, elems[base + 0], &v3[0])) return 0;
        if (!get_f32_term(env, elems[base + 1], &v3[1])) return 0;
        if (!get_f32_term(env, elems[base + 2], &v3[2])) return 0;

        if (!write_bytes(blob, blob_size, *off, v3, 12)) return 0;
        if (!write_zeros(blob, blob_size, *off + 12, 4)) return 0;
        *off += 16;
        return 1;
    }

    if (n == 9) {
        *off = align_up(*off, 16);

        float m[9];
        for (int i = 0; i < 9; i++) {
            if (!get_f32_term(env, elems[base + i], &m[i])) return 0;
        }

        for (int col = 0; col < 3; col++) {
            *off = align_up(*off, 16);
            float colv[3] = {
                m[0 * 3 + col],
                m[1 * 3 + col],
                m[2 * 3 + col],
            };
            if (!write_bytes(blob, blob_size, *off, colv, 12)) return 0;
            if (!write_zeros(blob, blob_size, *off + 12, 4)) return 0;
            *off += 16;
        }
        return 1;
    }

    if (n == 16) {
        *off = align_up(*off, 16);

        float m[16];
        for (int i = 0; i < 16; i++) {
            if (!get_f32_term(env, elems[base + i], &m[i])) return 0;
        }

        for (int col = 0; col < 4; col++) {
            *off = align_up(*off, 16);
            float colv[4] = {
                m[0 * 4 + col],
                m[1 * 4 + col],
                m[2 * 4 + col],
                m[3 * 4 + col],
            };
            if (!write_bytes(blob, blob_size, *off, colv, 16)) return 0;
            *off += 16;
        }
        return 1;
    }

    (void)tagged;

    return 0;
}

int pack_std140_params_term(ErlNifEnv* env, ERL_NIF_TERM params_term,
                            uint8_t* blob, size_t blob_size) {
    const ERL_NIF_TERM* elems = NULL;

    if (enif_is_identical(params_term, enif_make_atom(env, "nil"))) {
        memset(blob, 0, blob_size);
        return 1;
    }

    size_t off = 0;
    memset(blob, 0, blob_size);

    int arity = 0;

    if (enif_get_tuple(env, params_term, &arity, &elems)) {
        for (int i = 0; i < arity; i++) {
            if (!pack_std140_term(env, elems[i], blob, blob_size, &off))
                return 0;
        }
        return off <= blob_size;
    }

    if (!pack_std140_term(env, params_term, blob, blob_size, &off)) return 0;

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
