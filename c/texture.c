#include "texture.h"

#include <stdatomic.h>

#include "device.h"

static ErlNifResourceType* TEXTURE_RES_TYPE = NULL;

static atomic_uint_fast32_t g_next_texture_index = 0;

static uint32_t alloc_texture_index(void) {
    uint32_t idx = atomic_fetch_add(&g_next_texture_index, 1);

    if (idx >= MAX_BINDLESS_RESOURCES) {
        enif_fprintf(stderr, "texture: MAX_BINDLESS_RESOURCES exceeded (%u)\n",
                     (unsigned)MAX_BINDLESS_RESOURCES);
        return UINT32_MAX;
    }

    return idx;
}

static void texture_res_dtor(ErlNifEnv* env, void* obj) {
    (void)env;

    vkDeviceWaitIdle(g_device.logical_device);

    texture_res_t* tex = (texture_res_t*)obj;
    if (!tex) return;

    if (tex->sampler != VK_NULL_HANDLE) {
        vkDestroySampler(g_device.logical_device, tex->sampler, NULL);
        tex->sampler = VK_NULL_HANDLE;
    }

    if (tex->image) {
        enif_release_resource(tex->image);
        tex->image = NULL;
    }

    tex->texture_index = 0;
}

int nif_init_texture_res(ErlNifEnv* env) {
    TEXTURE_RES_TYPE = enif_open_resource_type(
        env, "fluo_nif", "texture_res", texture_res_dtor,
        ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL);

    if (!TEXTURE_RES_TYPE) return -1;
    return 0;
}

texture_res_t* get_texture_from_term(ErlNifEnv* env, ERL_NIF_TERM term) {
    if (!env) return NULL;

    ERL_NIF_TERM handle_term = term;

    const ERL_NIF_TERM* elems = NULL;
    int arity = 0;

    if (enif_get_tuple(env, term, &arity, &elems)) {
        if (arity != 3) return NULL;

        if (!enif_is_atom(env, elems[0])) return NULL;

        ERL_NIF_TERM a_texture = enif_make_atom(env, "texture");
        if (!enif_is_identical(elems[0], a_texture)) return NULL;

        handle_term = elems[2];
    }

    texture_res_t* res = NULL;
    if (!enif_get_resource(env, handle_term, TEXTURE_RES_TYPE, (void**)&res))
        return NULL;

    return res;
}

static int create_default_sampler(VkSampler* out_sampler) {
    if (!out_sampler) return 0;

    VkSamplerCreateInfo si = {
        .sType = VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO,
        .pNext = NULL,
        .flags = 0,

        .magFilter = VK_FILTER_LINEAR,
        .minFilter = VK_FILTER_LINEAR,

        .mipmapMode = VK_SAMPLER_MIPMAP_MODE_LINEAR,
        .addressModeU = VK_SAMPLER_ADDRESS_MODE_REPEAT,
        .addressModeV = VK_SAMPLER_ADDRESS_MODE_REPEAT,
        .addressModeW = VK_SAMPLER_ADDRESS_MODE_REPEAT,

        .mipLodBias = 0.0f,
        .anisotropyEnable = VK_FALSE,
        .maxAnisotropy = 1.0f,

        .compareEnable = VK_FALSE,
        .compareOp = VK_COMPARE_OP_ALWAYS,

        .minLod = 0.0f,
        .maxLod = 0.0f,
        .borderColor = VK_BORDER_COLOR_INT_OPAQUE_BLACK,
        .unnormalizedCoordinates = VK_FALSE,
    };

    if (vkCreateSampler(g_device.logical_device, &si, NULL, out_sampler) !=
        VK_SUCCESS) {
        *out_sampler = VK_NULL_HANDLE;
        return 0;
    }

    return 1;
}

ERL_NIF_TERM nif_create_texture(ErlNifEnv* env, int argc,
                                const ERL_NIF_TERM argv[]) {
    if (argc != 3) return enif_make_badarg(env);

    ErlNifBinary bin = {0};
    if (!enif_inspect_binary(env, argv[0], &bin)) return enif_make_badarg(env);

    unsigned int width = 0, height = 0;
    if (!enif_get_uint(env, argv[1], &width)) return enif_make_badarg(env);
    if (!enif_get_uint(env, argv[2], &height)) return enif_make_badarg(env);
    if (width == 0 || height == 0) return enif_make_badarg(env);

    const size_t expected = (size_t)width * (size_t)height * 4u;
    if (bin.size != expected) {
        enif_fprintf(stderr,
                     "texture: bad pixel buffer size: got %zu, expected %zu "
                     "(%ux%u RGBA8)\n",
                     (size_t)bin.size, expected, width, height);
        return enif_make_badarg(env);
    }

    texture_res_t* tex = (texture_res_t*)enif_alloc_resource(
        TEXTURE_RES_TYPE, sizeof(texture_res_t));
    if (!tex) return enif_make_atom(env, "error");

    *tex = (texture_res_t){0};

    image_res_t* img = alloc_image_res(env);
    if (!img) {
        enif_release_resource(tex);
        return enif_make_atom(env, "error");
    }

    uint32_t slot = alloc_texture_index();
    if (slot == UINT32_MAX) {
        enif_release_resource(img);
        enif_release_resource(tex);
        return enif_make_atom(env, "error");
    }
    tex->texture_index = slot;

    if (!create_default_sampler(&tex->sampler)) {
        enif_release_resource(img);
        enif_release_resource(tex);
        return enif_make_atom(env, "error");
    }

    VkImageUsageFlags usage =
        VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT | VK_IMAGE_USAGE_SAMPLED_BIT |
        VK_IMAGE_USAGE_TRANSFER_DST_BIT | VK_IMAGE_USAGE_TRANSFER_SRC_BIT;

    if (!create_image(img, (uint32_t)width, (uint32_t)height,
                      VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
                      VK_FORMAT_R8G8B8A8_UNORM, usage,
                      VK_IMAGE_ASPECT_COLOR_BIT, 0)) {
        vkDestroySampler(g_device.logical_device, tex->sampler, NULL);
        tex->sampler = VK_NULL_HANDLE;
        enif_release_resource(img);
        enif_release_resource(tex);
        return enif_make_atom(env, "error");
    }

    VkBuffer staging_buf = VK_NULL_HANDLE;
    VmaAllocation staging_alloc = VK_NULL_HANDLE;

    VkBufferCreateInfo buf_info = {
        .sType = VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO,
        .size = (VkDeviceSize)expected,
        .usage = VK_BUFFER_USAGE_TRANSFER_SRC_BIT,
        .sharingMode = VK_SHARING_MODE_EXCLUSIVE,
    };

    VmaAllocationCreateInfo alloc_info = {
        .usage = VMA_MEMORY_USAGE_AUTO,
        .flags = VMA_ALLOCATION_CREATE_HOST_ACCESS_SEQUENTIAL_WRITE_BIT,
    };

    if (vmaCreateBuffer(g_device.allocator, &buf_info, &alloc_info,
                        &staging_buf, &staging_alloc, NULL) != VK_SUCCESS) {
        vkDestroySampler(g_device.logical_device, tex->sampler, NULL);
        tex->sampler = VK_NULL_HANDLE;
        enif_release_resource(img);
        enif_release_resource(tex);
        return enif_make_atom(env, "error");
    }

    void* mapped = NULL;
    if (vmaMapMemory(g_device.allocator, staging_alloc, &mapped) !=
            VK_SUCCESS ||
        !mapped) {
        vmaDestroyBuffer(g_device.allocator, staging_buf, staging_alloc);
        vkDestroySampler(g_device.logical_device, tex->sampler, NULL);
        tex->sampler = VK_NULL_HANDLE;
        enif_release_resource(img);
        enif_release_resource(tex);
        return enif_make_atom(env, "error");
    }

    memcpy(mapped, bin.data, expected);
    vmaFlushAllocation(g_device.allocator, staging_alloc, 0,
                       (VkDeviceSize)expected);
    vmaUnmapMemory(g_device.allocator, staging_alloc);

    VkCommandBuffer cmd = begin_single_time_commands();

    transition_image_layout(img, VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL, cmd);

    VkBufferImageCopy region = {
        .bufferOffset = 0,
        .bufferRowLength = 0,
        .bufferImageHeight = 0,
        .imageSubresource =
            {
                .aspectMask = VK_IMAGE_ASPECT_COLOR_BIT,
                .mipLevel = 0,
                .baseArrayLayer = 0,
                .layerCount = 1,
            },
        .imageOffset = {0, 0, 0},
        .imageExtent = {(uint32_t)width, (uint32_t)height, 1},
    };

    vkCmdCopyBufferToImage(cmd, staging_buf, img->image,
                           VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL, 1, &region);

    transition_image_layout(img, VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL, cmd);

    if (!end_single_time_commands(cmd)) {
        vmaDestroyBuffer(g_device.allocator, staging_buf, staging_alloc);
        vkDestroySampler(g_device.logical_device, tex->sampler, NULL);
        tex->sampler = VK_NULL_HANDLE;
        enif_release_resource(img);
        enif_release_resource(tex);
        return enif_make_atom(env, "error");
    }

    vmaDestroyBuffer(g_device.allocator, staging_buf, staging_alloc);

    VkDescriptorImageInfo ii = {
        .sampler = tex->sampler,
        .imageView = img->view,
        .imageLayout = VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
    };

    VkWriteDescriptorSet w = {
        .sType = VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET,
        .dstSet = g_device.descriptor_set,
        .dstBinding = SAMPLER_BINDING,
        .dstArrayElement = tex->texture_index,
        .descriptorCount = 1,
        .descriptorType = VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
        .pImageInfo = &ii,
    };

    vkUpdateDescriptorSets(g_device.logical_device, 1, &w, 0, NULL);

    tex->image = img;
    enif_keep_resource(img);

    ERL_NIF_TERM img_term = enif_make_resource(env, img);
    enif_release_resource(img);

    ERL_NIF_TERM tex_term = enif_make_resource(env, tex);
    enif_release_resource(tex);

    return enif_make_tuple2(env, img_term, tex_term);
}
