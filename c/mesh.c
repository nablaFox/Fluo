#include "mesh.h"

#include "buffer.h"
#include "utils.h"

static ErlNifResourceType* MESH_RES_TYPE = NULL;

static void mesh_res_dtor(ErlNifEnv* env, void* obj) {
    (void)env;
    mesh_res_t* m = (mesh_res_t*)obj;

    destroy_gpu_buffer(&m->vertex_buffer);
    destroy_gpu_buffer(&m->index_buffer);
}

int nif_init_mesh_res(ErlNifEnv* env) {
    MESH_RES_TYPE =
        enif_open_resource_type(env, "fluo_nif", "mesh_res", mesh_res_dtor,
                                ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL);

    return MESH_RES_TYPE ? 0 : -1;
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
        .vertex_buffer = (GpuBuffer){0},
        .index_buffer = (GpuBuffer){0},
    };

    const VkDeviceSize vsize =
        (VkDeviceSize)sizeof(VertexGPU) * (VkDeviceSize)vcount;
    const VkDeviceSize isize =
        (VkDeviceSize)sizeof(uint32_t) * (VkDeviceSize)icount;

    const VkBufferUsageFlags v_usage = VK_BUFFER_USAGE_STORAGE_BUFFER_BIT |
                                       VK_BUFFER_USAGE_VERTEX_BUFFER_BIT |
                                       VK_BUFFER_USAGE_TRANSFER_DST_BIT;

    const VkBufferUsageFlags i_usage =
        VK_BUFFER_USAGE_STORAGE_BUFFER_BIT | VK_BUFFER_USAGE_TRANSFER_DST_BIT;

    const VkPipelineStageFlags shader_stages =
        VK_PIPELINE_STAGE_VERTEX_SHADER_BIT |
        VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT |
        VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT;

    const VkAccessFlags shader_access = VK_ACCESS_SHADER_READ_BIT;

    const int vcreate = create_gpu_buffer(&res->vertex_buffer, vsize, v_usage,
                                          VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT);
    const int icreate = create_gpu_buffer(&res->index_buffer, isize, i_usage,
                                          VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT);

    if (!vcreate || !icreate) {
        destroy_gpu_buffer(&res->vertex_buffer);
        destroy_gpu_buffer(&res->index_buffer);
        enif_free(indices);
        enif_free(vertices);
        enif_release_resource(res);
        return enif_make_badarg(env);
    }

    const int vok = write_gpu_buffer(&res->vertex_buffer, vertices, vsize, 0,
                                     shader_stages, shader_access);

    const int iok = write_gpu_buffer(&res->index_buffer, indices, isize, 0,
                                     shader_stages, shader_access);

    enif_free(indices);
    enif_free(vertices);

    if (!vok || !iok) {
        destroy_gpu_buffer(&res->vertex_buffer);
        destroy_gpu_buffer(&res->index_buffer);
        enif_release_resource(res);
        return enif_make_badarg(env);
    }

    ERL_NIF_TERM handle_term = enif_make_resource(env, res);
    enif_release_resource(res);

    return handle_term;
}

mesh_res_t* get_mesh_from_term(ErlNifEnv* env, ERL_NIF_TERM term) {
    if (!env) return NULL;

    mesh_res_t* res = NULL;
    ERL_NIF_TERM handle_term = term;

    const ERL_NIF_TERM* elems = NULL;
    int arity = 0;

    if (enif_get_tuple(env, term, &arity, &elems) && arity > 0) {
        handle_term = elems[arity - 1];
    }

    if (!enif_get_resource(env, handle_term, MESH_RES_TYPE, (void**)&res))
        return NULL;

    if (!res) return NULL;

    if (res->vertices_count == 0 || res->indices_count == 0) return NULL;
    if (res->vertex_buffer.buffer == VK_NULL_HANDLE) return NULL;
    if (res->index_buffer.buffer == VK_NULL_HANDLE) return NULL;

    return res;
}
