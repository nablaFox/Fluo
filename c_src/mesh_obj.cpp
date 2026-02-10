#include "mesh.h"

#include <unordered_map>

#define TINYOBJLOADER_IMPLEMENTATION
#include "tiny_obj_loader.h"

struct VNTKey {
    int v;
    int n;
    int t;

    bool operator==(const VNTKey& other) const noexcept { return v == other.v && n == other.n && t == other.t; }
};

struct VNTKeyHash {
    std::size_t operator()(const VNTKey& k) const noexcept {
        std::size_t h = std::hash<int>{}(k.v);
        h ^= std::hash<int>{}(k.n) + 0x9e3779b9 + (h << 6) + (h >> 2);
        h ^= std::hash<int>{}(k.t) + 0x9e3779b9 + (h << 6) + (h >> 2);
        return h;
    }
};

static mesh_res_t* load_mesh_from_obj_cpp(const char* path) {
    if (!path || !path[0])
        return nullptr;

    tinyobj::attrib_t attrib;
    std::vector<tinyobj::shape_t> shapes;
    std::vector<tinyobj::material_t> materials;

    std::string warn;
    std::string err;

    bool ok = tinyobj::LoadObj(&attrib, &shapes, &materials, &warn, &err, path, nullptr, true);

    if (!warn.empty()) {
    }

    if (!ok || !err.empty()) {
        return nullptr;
    }

    if (shapes.empty() || attrib.vertices.empty())
        return nullptr;

    std::vector<VertexGPU> vertices;
    std::vector<uint32_t> indices;

    vertices.reserve(1024);
    indices.reserve(2048);

    std::unordered_map<VNTKey, uint32_t, VNTKeyHash> dedup;
    dedup.reserve(4096);

    for (const auto& shape : shapes) {
        for (const auto& idx : shape.mesh.indices) {
            VNTKey key{idx.vertex_index, idx.normal_index, idx.texcoord_index};

            auto it = dedup.find(key);
            uint32_t out_index;

            if (it == dedup.end()) {
                VertexGPU v{};

                if (key.v < 0 || (size_t)(3 * key.v + 2) >= attrib.vertices.size())
                    return nullptr;

                v.pos[0] = attrib.vertices[3 * key.v + 0];
                v.pos[1] = attrib.vertices[3 * key.v + 1];
                v.pos[2] = attrib.vertices[3 * key.v + 2];

                if (key.n >= 0 && (size_t)(3 * key.n + 2) < attrib.normals.size()) {
                    v.normal[0] = attrib.normals[3 * key.n + 0];
                    v.normal[1] = attrib.normals[3 * key.n + 1];
                    v.normal[2] = attrib.normals[3 * key.n + 2];
                } else {
                    v.normal[0] = v.normal[1] = v.normal[2] = 0.0f;
                }

                if (key.t >= 0 && (size_t)(2 * key.t + 1) < attrib.texcoords.size()) {
                    v.uv[0] = attrib.texcoords[2 * key.t + 0];
                    v.uv[1] = attrib.texcoords[2 * key.t + 1];
                } else {
                    v.uv[0] = v.uv[1] = 0.0f;
                }

                out_index = static_cast<uint32_t>(vertices.size());
                vertices.push_back(v);
                dedup.emplace(key, out_index);
            } else {
                out_index = it->second;
            }

            indices.push_back(out_index);
        }
    }

    if (vertices.empty() || indices.empty())
        return nullptr;

    return create_mesh(vertices.data(), static_cast<uint32_t>(vertices.size()), indices.data(),
                       static_cast<uint32_t>(indices.size()));
}

extern "C" ERL_NIF_TERM nif_load_mesh_from_obj(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 1)
        return enif_make_badarg(env);

    ErlNifBinary bin;

    if (!enif_inspect_binary(env, argv[0], &bin))
        return enif_make_badarg(env);

    if (bin.size == 0 || bin.size >= 1024)
        return enif_make_badarg(env);

    char path[1024];
    memcpy(path, bin.data, bin.size);
    path[bin.size] = '\0';

    mesh_res_t* res = load_mesh_from_obj_cpp(path);

    if (!res)
        return enif_make_badarg(env);

    ERL_NIF_TERM handle_term = enif_make_resource(env, res);
    enif_release_resource(res);
    return handle_term;
}
