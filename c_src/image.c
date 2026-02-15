#include "image.h"

#include "device.h"

static ErlNifResourceType* IMAGE_RES_TYPE = NULL;

VkCommandPool g_blit_cmd_pool = VK_NULL_HANDLE;

static ERL_NIF_TERM ATOM_NONE;
static ERL_NIF_TERM ATOM_SOME;
static ERL_NIF_TERM ATOM_IMAGE_HANDLE;

int create_image(image_res_t* out, uint32_t width, uint32_t height,
                 VkImageLayout optimal_layout, VkFormat format,
                 VkImageUsageFlags usage, VkImageAspectFlags aspect,
                 VkMemoryPropertyFlags memory_properties,
                 VkSampleCountFlagBits samples) {
    if (!out || width == 0 || height == 0) return 0;

    *out = (image_res_t){0};

    VkImageCreateInfo img_info = {
        .sType = VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO,
        .pNext = NULL,
        .flags = 0,
        .imageType = VK_IMAGE_TYPE_2D,
        .format = format,
        .extent = (VkExtent3D){.width = width, .height = height, .depth = 1},
        .mipLevels = 1,
        .arrayLayers = 1,
        .samples = samples,
        .tiling = VK_IMAGE_TILING_OPTIMAL,
        .usage = usage,
        .sharingMode = VK_SHARING_MODE_EXCLUSIVE,
        .queueFamilyIndexCount = 0,
        .pQueueFamilyIndices = NULL,
        .initialLayout = VK_IMAGE_LAYOUT_UNDEFINED,
    };

    VmaAllocationCreateInfo alloc_info = {
        .flags = 0,
        .usage = VMA_MEMORY_USAGE_AUTO,
        .requiredFlags = memory_properties,
        .preferredFlags = 0,
        .memoryTypeBits = 0,
        .pool = VK_NULL_HANDLE,
        .pUserData = NULL,
        .priority = 0.0f,
    };

    if (vmaCreateImage(g_device.allocator, &img_info, &alloc_info, &out->image,
                       &out->alloc, NULL) != VK_SUCCESS) {
        *out = (image_res_t){0};
        return 0;
    }

    VkImageViewCreateInfo view_info = {
        .sType = VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO,
        .pNext = NULL,
        .flags = 0,
        .image = out->image,
        .viewType = VK_IMAGE_VIEW_TYPE_2D,
        .format = format,
        .components =
            (VkComponentMapping){
                .r = VK_COMPONENT_SWIZZLE_IDENTITY,
                .g = VK_COMPONENT_SWIZZLE_IDENTITY,
                .b = VK_COMPONENT_SWIZZLE_IDENTITY,
                .a = VK_COMPONENT_SWIZZLE_IDENTITY,
            },
        .subresourceRange =
            (VkImageSubresourceRange){
                .aspectMask = aspect,
                .baseMipLevel = 0,
                .levelCount = 1,
                .baseArrayLayer = 0,
                .layerCount = 1,
            },
    };

    if (vkCreateImageView(g_device.logical_device, &view_info, NULL,
                          &out->view) != VK_SUCCESS) {
        vmaDestroyImage(g_device.allocator, out->image, out->alloc);
        *out = (image_res_t){0};
        return 0;
    }

    out->aspect = aspect;
    out->format = format;
    out->current_layout = VK_IMAGE_LAYOUT_UNDEFINED;
    out->optimal_layout = optimal_layout;
    out->extent = (VkExtent2D){.width = width, .height = height};
    out->samples = samples;

    return 1;
}

int create_color_image(image_res_t* out, VkImageLayout optimal_layout,
                       uint32_t width, uint32_t height, VkImageUsageFlags usage,
                       VkMemoryPropertyFlags memory_properties) {
    return create_image(out, width, height, optimal_layout, FLUO_COLOR_FORMAT,
                        usage, VK_IMAGE_ASPECT_COLOR_BIT, memory_properties,
                        g_device.max_sample_count);
}

int create_depth_image(image_res_t* out, VkImageLayout optimal_layout,
                       uint32_t width, uint32_t height, VkImageUsageFlags usage,
                       VkMemoryPropertyFlags memory_properties) {
    return create_image(out, width, height, optimal_layout, FLUO_DEPTH_FORMAT,
                        usage, VK_IMAGE_ASPECT_DEPTH_BIT, memory_properties,
                        g_device.max_sample_count);
}

void destroy_gpu_image(image_res_t* img) {
    if (!img) return;

    if (img->view != VK_NULL_HANDLE) {
        vkDestroyImageView(g_device.logical_device, img->view, NULL);
        img->view = VK_NULL_HANDLE;
    }

    if (img->image != VK_NULL_HANDLE && img->alloc) {
        vmaDestroyImage(g_device.allocator, img->image, img->alloc);
    }

    *img = (image_res_t){0};
}

void blit_image(image_res_t src, image_res_t dst, VkCommandBuffer cmd) {
    if (src.image == VK_NULL_HANDLE || dst.image == VK_NULL_HANDLE) return;

    VkImageBlit region = {
        .srcSubresource =
            (VkImageSubresourceLayers){
                .aspectMask = src.aspect,
                .mipLevel = 0,
                .baseArrayLayer = 0,
                .layerCount = 1,
            },
        .srcOffsets =
            {
                {0, 0, 0},
                {(int32_t)src.extent.width, (int32_t)src.extent.height, 1},
            },
        .dstSubresource =
            (VkImageSubresourceLayers){
                .aspectMask = dst.aspect,
                .mipLevel = 0,
                .baseArrayLayer = 0,
                .layerCount = 1,
            },
        .dstOffsets =
            {
                {0, 0, 0},
                {(int32_t)dst.extent.width, (int32_t)dst.extent.height, 1},
            },
    };

    VkFilter filter =
        (src.aspect & (VK_IMAGE_ASPECT_DEPTH_BIT | VK_IMAGE_ASPECT_STENCIL_BIT))
            ? VK_FILTER_NEAREST
            : VK_FILTER_LINEAR;

    vkCmdBlitImage(cmd, src.image, VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
                   dst.image, VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL, 1, &region,
                   filter);
}

static void image_res_dtor(ErlNifEnv* env, void* obj) {
    (void)env;

    vkDeviceWaitIdle(g_device.logical_device);

    destroy_gpu_image((image_res_t*)obj);
}

int nif_init_image_res(ErlNifEnv* env) {
    IMAGE_RES_TYPE =
        enif_open_resource_type(env, "fluo_nif", "image_res", image_res_dtor,
                                ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL);

    if (!IMAGE_RES_TYPE) return -1;

    ATOM_NONE = enif_make_atom(env, "none");
    ATOM_SOME = enif_make_atom(env, "some");
    ATOM_IMAGE_HANDLE = enif_make_atom(env, "image_handle");

    if (g_blit_cmd_pool == VK_NULL_HANDLE) {
        VkCommandPoolCreateInfo pool_ci = {
            .sType = VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO,
            .flags = VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT,
            .queueFamilyIndex = g_device.graphics_family,
        };

        if (vkCreateCommandPool(g_device.logical_device, &pool_ci, NULL,
                                &g_blit_cmd_pool) != VK_SUCCESS) {
            g_blit_cmd_pool = VK_NULL_HANDLE;
            return -1;
        }
    }

    return 0;
}

image_res_t* alloc_image_res(ErlNifEnv* env) {
    if (!env || !IMAGE_RES_TYPE) return NULL;

    image_res_t* img =
        (image_res_t*)enif_alloc_resource(IMAGE_RES_TYPE, sizeof(image_res_t));

    if (!img) return NULL;

    *img = (image_res_t){0};

    return img;
}

image_res_t* get_image_from_term(ErlNifEnv* env, ERL_NIF_TERM term) {
    const ERL_NIF_TERM* elems = NULL;
    int arity = 0;

    if (!enif_get_tuple(env, term, &arity, &elems)) return NULL;
    if (arity != 2) return NULL;

    if (!enif_is_identical(elems[0], ATOM_IMAGE_HANDLE)) return NULL;

    image_res_t* res = NULL;
    if (!enif_get_resource(env, elems[1], IMAGE_RES_TYPE, (void**)&res))
        return NULL;

    return res;
}

image_res_t* get_image_from_option(ErlNifEnv* env, ERL_NIF_TERM term) {
    if (enif_is_identical(term, ATOM_NONE)) return NULL;

    const ERL_NIF_TERM* elems = NULL;
    int arity = 0;

    if (!enif_get_tuple(env, term, &arity, &elems)) return NULL;
    if (arity != 2) return NULL;

    if (!enif_is_identical(elems[0], ATOM_SOME)) return NULL;

    return get_image_from_term(env, elems[1]);
}

ERL_NIF_TERM nif_create_depth_image(ErlNifEnv* env, int argc,
                                    const ERL_NIF_TERM argv[]) {
    if (argc != 2) return enif_make_badarg(env);

    unsigned int width = 0;
    unsigned int height = 0;

    if (!enif_get_uint(env, argv[0], &width)) return enif_make_badarg(env);
    if (!enif_get_uint(env, argv[1], &height)) return enif_make_badarg(env);
    if (width == 0 || height == 0) return enif_make_badarg(env);

    image_res_t* img =
        (image_res_t*)enif_alloc_resource(IMAGE_RES_TYPE, sizeof(image_res_t));
    if (!img) return enif_make_badarg(env);

    *img = (image_res_t){0};

    if (!create_depth_image(img,
                            VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL,
                            width, height, FLUO_DEPTH_IMAGE_USAGE, 0)) {
        enif_release_resource(img);
        return enif_make_atom(env, "error");
    }

    ERL_NIF_TERM term = enif_make_resource(env, img);
    enif_release_resource(img);
    return term;
}

ERL_NIF_TERM nif_create_color_image(ErlNifEnv* env, int argc,
                                    const ERL_NIF_TERM argv[]) {
    if (argc != 2) return enif_make_badarg(env);

    unsigned int width = 0;
    unsigned int height = 0;

    if (!enif_get_uint(env, argv[0], &width)) return enif_make_badarg(env);
    if (!enif_get_uint(env, argv[1], &height)) return enif_make_badarg(env);
    if (width == 0 || height == 0) return enif_make_badarg(env);

    image_res_t* img =
        (image_res_t*)enif_alloc_resource(IMAGE_RES_TYPE, sizeof(image_res_t));

    if (!img) return enif_make_badarg(env);

    *img = (image_res_t){0};

    if (!create_color_image(img, VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
                            width, height, FLUO_COLOR_IMAGE_USAGE, 0)) {
        enif_release_resource(img);
        return enif_make_atom(env, "error");
    }

    ERL_NIF_TERM term = enif_make_resource(env, img);
    enif_release_resource(img);
    return term;
}

static VkDeviceSize bytes_per_pixel(const image_res_t* img) {
    switch (img->format) {
        case VK_FORMAT_R8G8B8A8_UNORM:
        case VK_FORMAT_B8G8R8A8_UNORM:
            return 4;

        case VK_FORMAT_R16G16B16A16_SFLOAT:
            return 8;

        case VK_FORMAT_R32G32B32A32_SFLOAT:
            return 16;

        default:
            return 0;
    }
}

ERL_NIF_TERM nif_read_image(ErlNifEnv* env, int argc,
                            const ERL_NIF_TERM argv[]) {
    if (argc != 1) return enif_make_badarg(env);

    image_res_t* img = NULL;

    if (!enif_get_resource(env, argv[0], IMAGE_RES_TYPE, (void**)&img) ||
        !img || img->image == VK_NULL_HANDLE) {
        return enif_make_badarg(env);
    }

    const uint32_t w = img->extent.width;
    const uint32_t h = img->extent.height;
    if (w == 0 || h == 0) return enif_make_badarg(env);

    const VkDeviceSize bpp = bytes_per_pixel(img);
    if (bpp == 0) return enif_make_badarg(env);

    const VkDeviceSize size = (VkDeviceSize)w * (VkDeviceSize)h * bpp;

    VkBuffer staging_buf = VK_NULL_HANDLE;
    VmaAllocation staging_alloc = VK_NULL_HANDLE;

    VkBufferCreateInfo buf_info = {
        .sType = VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO,
        .size = size,
        .usage = VK_BUFFER_USAGE_TRANSFER_DST_BIT,
        .sharingMode = VK_SHARING_MODE_EXCLUSIVE,
    };

    VmaAllocationCreateInfo alloc_info = {
        .usage = VMA_MEMORY_USAGE_AUTO,
        .flags = VMA_ALLOCATION_CREATE_HOST_ACCESS_RANDOM_BIT,
    };

    if (vmaCreateBuffer(g_device.allocator, &buf_info, &alloc_info,
                        &staging_buf, &staging_alloc, NULL) != VK_SUCCESS) {
        return enif_make_atom(env, "error");
    }

    VkCommandBuffer cmd = begin_single_time_commands();

    VkImageLayout old_layout = img->current_layout;

    transition_image_layout(img, VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL, cmd);

    VkBufferImageCopy region = {
        .bufferOffset = 0,
        .bufferRowLength = 0,
        .bufferImageHeight = 0,
        .imageSubresource =
            {
                .aspectMask = img->aspect,
                .mipLevel = 0,
                .baseArrayLayer = 0,
                .layerCount = 1,
            },
        .imageOffset = {0, 0, 0},
        .imageExtent = {w, h, 1},
    };

    vkCmdCopyImageToBuffer(cmd, img->image,
                           VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL, staging_buf, 1,
                           &region);

    transition_image_layout(img, old_layout, cmd);

    if (!end_single_time_commands(cmd)) {
        vmaDestroyBuffer(g_device.allocator, staging_buf, staging_alloc);
        return enif_make_atom(env, "error");
    }

    void* mapped = NULL;
    if (vmaMapMemory(g_device.allocator, staging_alloc, &mapped) !=
            VK_SUCCESS ||
        !mapped) {
        vmaDestroyBuffer(g_device.allocator, staging_buf, staging_alloc);
        return enif_make_atom(env, "error");
    }

    vmaInvalidateAllocation(g_device.allocator, staging_alloc, 0, size);

    ERL_NIF_TERM bin_term;
    unsigned char* bin = enif_make_new_binary(env, (size_t)size, &bin_term);
    memcpy(bin, mapped, (size_t)size);

    vmaUnmapMemory(g_device.allocator, staging_alloc);
    vmaDestroyBuffer(g_device.allocator, staging_buf, staging_alloc);

    return bin_term;
}

void destroy_blit_command_pool(void) {
    VkDevice dev = g_device.logical_device;
    if (!dev) return;

    if (g_blit_cmd_pool != VK_NULL_HANDLE) {
        vkDestroyCommandPool(dev, g_blit_cmd_pool, NULL);
        g_blit_cmd_pool = VK_NULL_HANDLE;
    }
}
