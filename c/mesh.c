#include "mesh.h"

#include <stdint.h>
#include <vulkan/vulkan.h>

#include "buffer.h"
#include "device.h"

static ErlNifResourceType* MESH_RES_TYPE = NULL;

typedef struct {
    uint32_t vertices_count;
    uint32_t indices_count;

    VkBuffer vertex_buf;
    VmaAllocation vertex_alloc;

    VkBuffer index_buf;
    VmaAllocation index_alloc;
} mesh_res_t;

static void mesh_res_dtor(ErlNifEnv* env, void* obj) {
    (void)env;
    mesh_res_t* m = (mesh_res_t*)obj;

    if (m->vertex_buf) {
        vmaDestroyBuffer(g_device.allocator, m->vertex_buf, m->vertex_alloc);
        m->vertex_buf = VK_NULL_HANDLE;
        m->vertex_alloc = NULL;
    }

    if (m->index_buf) {
        vmaDestroyBuffer(g_device.allocator, m->index_buf, m->index_alloc);
        m->index_buf = VK_NULL_HANDLE;
        m->index_alloc = NULL;
    }
}

int nif_init_mesh_res(ErlNifEnv* env) {
    MESH_RES_TYPE =
        enif_open_resource_type(env, "fluo_nif", "mesh_res", mesh_res_dtor,
                                ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL);

    if (!MESH_RES_TYPE) return -1;

    return 0;
}

static int is_tag(ErlNifEnv* env, ERL_NIF_TERM term, const char* a,
                  const char* b) {
    char buf[32];
    if (!enif_get_atom(env, term, buf, sizeof(buf), ERL_NIF_LATIN1)) return 0;
    return (strcmp(buf, a) == 0) || (b && strcmp(buf, b) == 0);
}

static int decode_f32(ErlNifEnv* env, ERL_NIF_TERM t, float* out) {
    double d;
    if (!enif_get_double(env, t, &d)) return 0;
    *out = (float)d;
    return 1;
}

static int decode_vec3(ErlNifEnv* env, ERL_NIF_TERM t, float* x, float* y,
                       float* z) {
    const ERL_NIF_TERM* e;
    int arity = 0;
    if (!enif_get_tuple(env, t, &arity, &e)) return 0;
    if (arity != 4) return 0;
    if (!is_tag(env, e[0], "vec3", "Vec3")) return 0;

    return decode_f32(env, e[1], x) && decode_f32(env, e[2], y) &&
           decode_f32(env, e[3], z);
}

static int decode_color(ErlNifEnv* env, ERL_NIF_TERM t, float* r, float* g,
                        float* b) {
    const ERL_NIF_TERM* e;
    int arity = 0;
    if (!enif_get_tuple(env, t, &arity, &e)) return 0;
    if (arity != 4) return 0;
    if (!is_tag(env, e[0], "color", "Color")) return 0;

    return decode_f32(env, e[1], r) && decode_f32(env, e[2], g) &&
           decode_f32(env, e[3], b);
}

static int decode_vertex(ErlNifEnv* env, ERL_NIF_TERM t, VertexGPU* out) {
    const ERL_NIF_TERM* e;
    int arity = 0;
    if (!enif_get_tuple(env, t, &arity, &e)) return 0;
    if (arity != 3) return 0;
    if (!is_tag(env, e[0], "vertex", "Vertex")) return 0;

    if (!decode_vec3(env, e[1], &out->px, &out->py, &out->pz)) return 0;
    if (!decode_color(env, e[2], &out->cr, &out->cg, &out->cb)) return 0;
    return 1;
}

ERL_NIF_TERM nif_create_mesh(ErlNifEnv* env, int argc,
                             const ERL_NIF_TERM argv[]) {
    if (argc != 2) return enif_make_badarg(env);

    const ERL_NIF_TERM vertices_list = argv[0];
    const ERL_NIF_TERM indices_list = argv[1];

    unsigned int vcount = 0;
    if (!enif_get_list_length(env, vertices_list, &vcount))
        return enif_make_badarg(env);

    if (vcount == 0) return enif_make_badarg(env);

    VertexGPU* vertices = (VertexGPU*)enif_alloc(sizeof(VertexGPU) * vcount);
    if (!vertices) return enif_make_badarg(env);

    ERL_NIF_TERM head, tail;
    tail = vertices_list;

    for (unsigned int i = 0; i < vcount; i++) {
        if (!enif_get_list_cell(env, tail, &head, &tail)) {
            enif_free(vertices);
            return enif_make_badarg(env);
        }
        if (!decode_vertex(env, head, &vertices[i])) {
            enif_free(vertices);
            return enif_make_badarg(env);
        }
    }

    unsigned int icount = 0;
    if (!enif_get_list_length(env, indices_list, &icount)) {
        enif_free(vertices);
        return enif_make_badarg(env);
    }
    if (icount == 0) {
        enif_free(vertices);
        return enif_make_badarg(env);
    }

    uint32_t* indices = (uint32_t*)enif_alloc(sizeof(uint32_t) * icount);
    if (!indices) {
        enif_free(vertices);
        return enif_make_badarg(env);
    }

    tail = indices_list;
    for (unsigned int i = 0; i < icount; i++) {
        if (!enif_get_list_cell(env, tail, &head, &tail)) {
            enif_free(indices);
            enif_free(vertices);
            return enif_make_badarg(env);
        }
        int idx = 0;
        if (!enif_get_int(env, head, &idx) || idx < 0) {
            enif_free(indices);
            enif_free(vertices);
            return enif_make_badarg(env);
        }
        indices[i] = (uint32_t)idx;
    }

    mesh_res_t* res =
        (mesh_res_t*)enif_alloc_resource(MESH_RES_TYPE, sizeof(mesh_res_t));

    if (!res) {
        enif_free(indices);
        enif_free(vertices);
        return enif_make_badarg(env);
    }

    *res = (mesh_res_t){
        .vertices_count = (uint32_t)vcount,
        .indices_count = (uint32_t)icount,
        .vertex_buf = VK_NULL_HANDLE,
        .vertex_alloc = NULL,
        .index_buf = VK_NULL_HANDLE,
        .index_alloc = NULL,
    };

    const VkDeviceSize vsize =
        (VkDeviceSize)sizeof(VertexGPU) * (VkDeviceSize)vcount;
    const VkDeviceSize isize =
        (VkDeviceSize)sizeof(uint32_t) * (VkDeviceSize)icount;

    const VkBufferUsageFlags v_usage =
        VK_BUFFER_USAGE_STORAGE_BUFFER_BIT | VK_BUFFER_USAGE_VERTEX_BUFFER_BIT;

    const VkBufferUsageFlags i_usage = VK_BUFFER_USAGE_STORAGE_BUFFER_BIT;

    const VkPipelineStageFlags shader_stages =
        VK_PIPELINE_STAGE_VERTEX_SHADER_BIT |
        VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT |
        VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT;

    const VkAccessFlags shader_access = VK_ACCESS_SHADER_READ_BIT;

    const int vok =
        write_gpu_buffer(vertices, vsize, v_usage, shader_stages, shader_access,
                         &res->vertex_buf, &res->vertex_alloc);

    const int iok =
        write_gpu_buffer(indices, isize, i_usage, shader_stages, shader_access,
                         &res->index_buf, &res->index_alloc);

    enif_free(indices);
    enif_free(vertices);

    if (!vok || !iok) {
        enif_release_resource(res);
        return enif_make_badarg(env);
    }

    const ERL_NIF_TERM handle_term = enif_make_resource(env, res);
    enif_release_resource(res);

    return handle_term;
}
