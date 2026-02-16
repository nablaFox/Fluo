#include "mesh.h"

#include "buffer.h"
#include "device.h"

static ErlNifResourceType* MESH_RES_TYPE = NULL;
static ErlNifResourceType* MESH_ALLOCATOR_RES_TYPE = NULL;

static ERL_NIF_TERM ATOM_MESH_HANDLE;

static void mesh_allocator_res_dtor(ErlNifEnv* env, void* obj) {
    (void)env;

    mesh_allocator_res_t* allocator = (mesh_allocator_res_t*)obj;

    vkDeviceWaitIdle(g_device.logical_device);

    destroy_gpu_buffer(&allocator->vertex_buffer);
    destroy_gpu_buffer(&allocator->index_buffer);
    destroy_gpu_buffer(&allocator->staging_buffer);
}

static void mesh_res_dtor(ErlNifEnv* env, void* obj) {
    (void)env;
    (void)obj;
}

int nif_init_mesh_res(ErlNifEnv* env) {
    MESH_ALLOCATOR_RES_TYPE = enif_open_resource_type(
        env, "fluo_nif", "mesh_allocator_res", mesh_allocator_res_dtor,
        ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL);

    MESH_RES_TYPE =
        enif_open_resource_type(env, "fluo_nif", "mesh_res", mesh_res_dtor,
                                ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL);

    ATOM_MESH_HANDLE = enif_make_atom(env, "mesh_handle");

    return (MESH_ALLOCATOR_RES_TYPE && MESH_RES_TYPE) ? 0 : -1;
}

ERL_NIF_TERM nif_create_mesh_allocator(ErlNifEnv* env, int argc,
                                       const ERL_NIF_TERM argv[]) {
    if (argc != 2) {
        return enif_make_badarg(env);
    }

    unsigned int vertices_count = 0;
    unsigned int indices_count = 0;

    if (!enif_get_uint(env, argv[0], &vertices_count)) {
        return enif_make_badarg(env);
    }

    if (!enif_get_uint(env, argv[1], &indices_count)) {
        return enif_make_badarg(env);
    }

    mesh_allocator_res_t* allocator = enif_alloc_resource(
        MESH_ALLOCATOR_RES_TYPE, sizeof(mesh_allocator_res_t));
    if (!allocator) {
        return enif_make_badarg(env);
    }

    VkDeviceSize vertex_buffer_size = vertices_count * sizeof(VertexGPU);
    VkDeviceSize index_buffer_size = indices_count * sizeof(uint32_t);
    VkDeviceSize staging_buffer_size = vertex_buffer_size + index_buffer_size;

    memset(allocator, 0, sizeof(mesh_allocator_res_t));

    allocator->vertex_heap = 0;
    allocator->index_heap = 0;
    allocator->vertex_buffer_size = vertex_buffer_size;
    allocator->index_buffer_size = index_buffer_size;

    if (!create_gpu_buffer(&allocator->vertex_buffer, vertex_buffer_size,
                           VK_BUFFER_USAGE_VERTEX_BUFFER_BIT |
                               VK_BUFFER_USAGE_TRANSFER_DST_BIT,
                           VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT)) {
        enif_release_resource(allocator);
        return enif_make_badarg(env);
    }

    if (!create_gpu_buffer(
            &allocator->index_buffer, index_buffer_size,
            VK_BUFFER_USAGE_INDEX_BUFFER_BIT | VK_BUFFER_USAGE_TRANSFER_DST_BIT,
            VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT)) {
        destroy_gpu_buffer(&allocator->vertex_buffer);
        enif_release_resource(allocator);
        return enif_make_badarg(env);
    }

    if (!create_gpu_buffer(&allocator->staging_buffer, staging_buffer_size,
                           VK_BUFFER_USAGE_TRANSFER_SRC_BIT,
                           VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT |
                               VK_MEMORY_PROPERTY_HOST_COHERENT_BIT)) {
        destroy_gpu_buffer(&allocator->vertex_buffer);
        destroy_gpu_buffer(&allocator->index_buffer);
        enif_release_resource(allocator);
        return enif_make_badarg(env);
    }

    ERL_NIF_TERM res_term = enif_make_resource(env, allocator);

    enif_release_resource(allocator);

    return res_term;
}

ERL_NIF_TERM nif_allocate_mesh(ErlNifEnv* env, int argc,
                               const ERL_NIF_TERM argv[]) {
    if (argc != 3) {
        return enif_make_badarg(env);
    }

    mesh_allocator_res_t* allocator = get_allocator_from_term(env, argv[0]);

    if (!allocator) {
        return enif_make_badarg(env);
    }

    unsigned int vertices_count = 0;
    unsigned int indices_count = 0;

    if (!enif_get_uint(env, argv[1], &vertices_count)) {
        return enif_make_badarg(env);
    }

    if (!enif_get_uint(env, argv[2], &indices_count)) {
        return enif_make_badarg(env);
    }

    VkDeviceSize vertex_size = vertices_count * sizeof(VertexGPU);
    VkDeviceSize index_size = indices_count * sizeof(uint32_t);

    mesh_res_t* mesh_res =
        enif_alloc_resource(MESH_RES_TYPE, sizeof(mesh_res_t));
    if (!mesh_res) {
        return enif_make_badarg(env);
    }

    enif_keep_resource(allocator);

    mesh_res->allocator = allocator;

    mesh_res->vertex_offset = allocator->vertex_heap;
    mesh_res->vertex_size = vertex_size;
    mesh_res->index_size = index_size;
    mesh_res->indices_count = indices_count;

    mesh_res->index_staging_offset =
        allocator->vertex_buffer_size + allocator->index_heap;

    mesh_res->index_offset = allocator->index_heap;

    allocator->vertex_heap += vertex_size;
    allocator->index_heap += index_size;

    ERL_NIF_TERM res_term = enif_make_resource(env, mesh_res);

    enif_release_resource(mesh_res);

    return res_term;
}

ERL_NIF_TERM nif_write_mesh(ErlNifEnv* env, int argc,
                            const ERL_NIF_TERM argv[]) {
    if (argc != 3) {
        return enif_make_badarg(env);
    }

    mesh_res_t* mesh = get_mesh_from_term(env, argv[0]);
    if (!mesh || !mesh->allocator) {
        return enif_make_badarg(env);
    }

    mesh_allocator_res_t* allocator = mesh->allocator;

    ErlNifBinary vertex_data;
    if (!enif_inspect_binary(env, argv[1], &vertex_data)) {
        return enif_make_badarg(env);
    }

    ErlNifBinary index_data;
    if (!enif_inspect_binary(env, argv[2], &index_data)) {
        return enif_make_badarg(env);
    }

    if (vertex_data.size != mesh->vertex_size) {
        return enif_make_badarg(env);
    }

    if (index_data.size != mesh->index_size) {
        return enif_make_badarg(env);
    }

    if (!write_gpu_buffer(&allocator->staging_buffer, vertex_data.data,
                          mesh->vertex_size, mesh->vertex_offset)) {
        return enif_make_badarg(env);
    }

    if (!write_gpu_buffer(&allocator->staging_buffer, index_data.data,
                          mesh->index_size, mesh->index_staging_offset)) {
        return enif_make_badarg(env);
    }

    return enif_make_atom(env, "ok");
}

ERL_NIF_TERM nif_submit_mesh_writes(ErlNifEnv* env, int argc,
                                    const ERL_NIF_TERM argv[]) {
    if (argc != 1) {
        return enif_make_badarg(env);
    }

    mesh_allocator_res_t* allocator = get_allocator_from_term(env, argv[0]);
    if (!allocator) {
        return enif_make_badarg(env);
    }

    if (!copy_gpu_buffer(&allocator->vertex_buffer, 0,
                         &allocator->staging_buffer, 0,
                         allocator->vertex_heap)) {
        return enif_make_badarg(env);
    }

    if (!copy_gpu_buffer(
            &allocator->index_buffer, 0, &allocator->staging_buffer,
            allocator->vertex_buffer_size, allocator->index_heap)) {
        return enif_make_badarg(env);
    }

    return enif_make_atom(env, "ok");
}

mesh_res_t* get_mesh_from_term(ErlNifEnv* env, ERL_NIF_TERM term) {
    const ERL_NIF_TERM* elems = NULL;
    int arity = 0;

    if (!enif_get_tuple(env, term, &arity, &elems)) return NULL;

    if (!enif_is_identical(elems[0], ATOM_MESH_HANDLE)) return NULL;

    mesh_res_t* res = NULL;

    if (!enif_get_resource(env, elems[1], MESH_RES_TYPE, (void**)&res))
        return NULL;

    return res;
}

mesh_allocator_res_t* get_allocator_from_term(ErlNifEnv* env,
                                              ERL_NIF_TERM term) {
    mesh_allocator_res_t* res = NULL;

    if (!enif_get_resource(env, term, MESH_ALLOCATOR_RES_TYPE, (void**)&res))
        return NULL;

    if (!res) return NULL;

    return res;
}
