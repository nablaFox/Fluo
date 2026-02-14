#include "mesh.h"

#include "buffer.h"
#include "device.h"
#include "utils.h"

static ErlNifResourceType* MESH_RES_TYPE = NULL;

static ERL_NIF_TERM ATOM_MESH_HANDLE;
static ERL_NIF_TERM ATOM_VEC2;
static ERL_NIF_TERM ATOM_VEC3;
static ERL_NIF_TERM ATOM_VERTEX;

static void mesh_res_dtor(ErlNifEnv* env, void* obj) {
    (void)env;

    mesh_res_t* m = (mesh_res_t*)obj;

    vkDeviceWaitIdle(g_device.logical_device);

    destroy_gpu_buffer(&m->vertex_buffer);
    destroy_gpu_buffer(&m->index_buffer);
}

int nif_init_mesh_res(ErlNifEnv* env) {
    MESH_RES_TYPE =
        enif_open_resource_type(env, "fluo_nif", "mesh_res", mesh_res_dtor,
                                ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL);

    ATOM_MESH_HANDLE = enif_make_atom(env, "mesh_handle");
    ATOM_VEC2 = enif_make_atom(env, "vec2");
    ATOM_VEC3 = enif_make_atom(env, "vec3");
    ATOM_VERTEX = enif_make_atom(env, "vertex");

    return MESH_RES_TYPE ? 0 : -1;
}

static int decode_vec3(ErlNifEnv* env, ERL_NIF_TERM t, float* x, float* y,
                       float* z) {
    const ERL_NIF_TERM* e = NULL;
    int arity = 0;
    if (!enif_get_tuple(env, t, &arity, &e)) return 0;
    if (arity != 4) return 0;

    if (!enif_is_identical(e[0], ATOM_VEC3)) return 0;

    return decode_f32(env, e[1], x) && decode_f32(env, e[2], y) &&
           decode_f32(env, e[3], z);
}

static int decode_vec2(ErlNifEnv* env, ERL_NIF_TERM t, float* x, float* y) {
    const ERL_NIF_TERM* e = NULL;
    int arity = 0;
    if (!enif_get_tuple(env, t, &arity, &e)) return 0;
    if (arity != 3) return 0;

    if (!enif_is_identical(e[0], ATOM_VEC2)) return 0;

    double dx = 0.0, dy = 0.0;
    if (!enif_get_double(env, e[1], &dx)) return 0;
    if (!enif_get_double(env, e[2], &dy)) return 0;

    *x = (float)dx;
    *y = (float)dy;
    return 1;
}

static int decode_vertex(ErlNifEnv* env, ERL_NIF_TERM t, VertexGPU* out) {
    const ERL_NIF_TERM* e = NULL;
    int arity = 0;
    if (!enif_get_tuple(env, t, &arity, &e)) return 0;
    if (arity != 4) return 0;

    if (!enif_is_identical(e[0], ATOM_VERTEX)) return 0;

    if (!decode_vec3(env, e[1], &out->pos[0], &out->pos[1], &out->pos[2]))
        return 0;
    if (!decode_vec3(env, e[2], &out->normal[0], &out->normal[1],
                     &out->normal[2]))
        return 0;
    if (!decode_vec2(env, e[3], &out->uv[0], &out->uv[1])) return 0;

    return 1;
}

ERL_NIF_TERM nif_create_mesh(ErlNifEnv* env, int argc,
                             const ERL_NIF_TERM argv[]) {
    if (argc != 2) return enif_make_badarg(env);

    const ERL_NIF_TERM vertices_list = argv[0];
    const ERL_NIF_TERM indices_list = argv[1];

    unsigned int vcount = 0;
    if (!enif_get_list_length(env, vertices_list, &vcount) || vcount == 0)
        return enif_make_badarg(env);

    VertexGPU* vertices = (VertexGPU*)enif_alloc(sizeof(VertexGPU) * vcount);
    if (!vertices) return enif_make_badarg(env);

    ERL_NIF_TERM head, tail = vertices_list;
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
    if (!enif_get_list_length(env, indices_list, &icount) || icount == 0) {
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
        create_mesh(vertices, (uint32_t)vcount, indices, (uint32_t)icount);

    enif_free(indices);
    enif_free(vertices);

    if (!res) return enif_make_badarg(env);

    ERL_NIF_TERM handle_term = enif_make_resource(env, res);
    enif_release_resource(res);
    return handle_term;
}

mesh_res_t* get_mesh_from_term(ErlNifEnv* env, ERL_NIF_TERM term) {
    const ERL_NIF_TERM* elems = NULL;
    int arity = 0;

    if (!enif_get_tuple(env, term, &arity, &elems)) return NULL;
    if (arity != 2) return NULL;

    if (!enif_is_identical(elems[0], ATOM_MESH_HANDLE)) return NULL;

    mesh_res_t* res = NULL;
    if (!enif_get_resource(env, elems[1], MESH_RES_TYPE, (void**)&res))
        return NULL;

    return res;
}

mesh_res_t* create_mesh(const VertexGPU* vertices, uint32_t vcount,
                        const uint32_t* indices, uint32_t icount) {
    if (!vertices || !indices) return NULL;
    if (vcount == 0 || icount == 0) return NULL;

    mesh_res_t* res =
        (mesh_res_t*)enif_alloc_resource(MESH_RES_TYPE, sizeof(mesh_res_t));
    if (!res) return NULL;

    *res = (mesh_res_t){
        .vertices_count = vcount,
        .indices_count = icount,
        .vertex_buffer = (GpuBuffer){0},
        .index_buffer = (GpuBuffer){0},
    };

    const VkDeviceSize vsize =
        (VkDeviceSize)sizeof(VertexGPU) * (VkDeviceSize)vcount;
    const VkDeviceSize isize =
        (VkDeviceSize)sizeof(uint32_t) * (VkDeviceSize)icount;

    const VkPipelineStageFlags shader_stages =
        VK_PIPELINE_STAGE_VERTEX_SHADER_BIT |
        VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT |
        VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT;

    const VkAccessFlags shader_access = VK_ACCESS_SHADER_READ_BIT;

    const int vcreate = create_gpu_buffer(
        &res->vertex_buffer, vsize,
        VK_BUFFER_USAGE_VERTEX_BUFFER_BIT | VK_BUFFER_USAGE_TRANSFER_DST_BIT,
        VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT);

    const int icreate = create_gpu_buffer(
        &res->index_buffer, isize,
        VK_BUFFER_USAGE_INDEX_BUFFER_BIT | VK_BUFFER_USAGE_TRANSFER_DST_BIT,
        VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT);

    if (!vcreate || !icreate) {
        destroy_gpu_buffer(&res->vertex_buffer);
        destroy_gpu_buffer(&res->index_buffer);
        enif_release_resource(res);
        return NULL;
    }

    const int vok = write_gpu_buffer(&res->vertex_buffer, vertices, vsize, 0,
                                     shader_stages, shader_access);

    const int iok = write_gpu_buffer(&res->index_buffer, indices, isize, 0,
                                     shader_stages, shader_access);

    if (!vok || !iok) {
        destroy_gpu_buffer(&res->vertex_buffer);
        destroy_gpu_buffer(&res->index_buffer);
        enif_release_resource(res);
        return NULL;
    }

    return res;
}
