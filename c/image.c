#include "image.h"

#include "device.h"

typedef struct {
    VkAccessFlags srcAccessMask;
    VkAccessFlags dstAccessMask;
    VkPipelineStageFlags srcStage;
    VkPipelineStageFlags dstStage;
} TransitionInfo;

static ErlNifResourceType* IMAGE_RES_TYPE = NULL;

static TransitionInfo get_transition_info(VkImageLayout oldLayout,
                                          VkImageLayout newLayout) {
    TransitionInfo info = {0};

    // UNDEFINED -> ANY (you were using TOP->TOP)
    if (oldLayout == VK_IMAGE_LAYOUT_UNDEFINED) {
        info.srcAccessMask = 0;
        info.dstAccessMask = 0;
        info.srcStage = VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT;
        info.dstStage = VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT;
    }

    // PRESENT -> TRANSFER_DST
    else if (oldLayout == VK_IMAGE_LAYOUT_PRESENT_SRC_KHR &&
             newLayout == VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL) {
        info.srcAccessMask = 0;
        info.dstAccessMask = VK_ACCESS_TRANSFER_WRITE_BIT;
        info.srcStage = VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT;
        info.dstStage = VK_PIPELINE_STAGE_TRANSFER_BIT;
    }

    // TRANSFER_DST -> PRESENT
    else if (oldLayout == VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL &&
             newLayout == VK_IMAGE_LAYOUT_PRESENT_SRC_KHR) {
        info.srcAccessMask = VK_ACCESS_TRANSFER_WRITE_BIT;
        info.dstAccessMask = 0;
        info.srcStage = VK_PIPELINE_STAGE_TRANSFER_BIT;
        info.dstStage = VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT;
    }

    // TRANSFER_SRC -> COLOR_ATTACHMENT
    else if (oldLayout == VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL &&
             newLayout == VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL) {
        info.srcAccessMask = VK_ACCESS_TRANSFER_READ_BIT;
        info.dstAccessMask = VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT;
        info.srcStage = VK_PIPELINE_STAGE_TRANSFER_BIT;
        info.dstStage = VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT;
    }

    // GENERAL -> ANY (you were using TOP->TOP)
    else if (oldLayout == VK_IMAGE_LAYOUT_GENERAL) {
        info.srcAccessMask = 0;
        info.dstAccessMask = 0;
        info.srcStage = VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT;
        info.dstStage = VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT;
    }

    // TRANSFER_DST -> COLOR_ATTACHMENT
    else if (oldLayout == VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL &&
             newLayout == VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL) {
        info.srcAccessMask = VK_ACCESS_TRANSFER_WRITE_BIT;
        info.dstAccessMask = VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT;
        info.srcStage = VK_PIPELINE_STAGE_TRANSFER_BIT;
        info.dstStage = VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT;
    }

    // COLOR_ATTACHMENT -> TRANSFER_DST
    else if (oldLayout == VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL &&
             newLayout == VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL) {
        info.srcAccessMask = VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT;
        info.dstAccessMask = VK_ACCESS_TRANSFER_WRITE_BIT;
        info.srcStage = VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT;
        info.dstStage = VK_PIPELINE_STAGE_TRANSFER_BIT;
    }

    // COLOR_ATTACHMENT -> TRANSFER_SRC
    else if (oldLayout == VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL &&
             newLayout == VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL) {
        info.srcAccessMask = VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT;
        info.dstAccessMask = VK_ACCESS_TRANSFER_READ_BIT;
        info.srcStage = VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT;
        info.dstStage = VK_PIPELINE_STAGE_TRANSFER_BIT;
    }

    // TRANSFER_DST -> DEPTH_STENCIL_ATTACHMENT
    else if (oldLayout == VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL &&
             newLayout == VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL) {
        info.srcAccessMask = VK_ACCESS_TRANSFER_WRITE_BIT;
        info.dstAccessMask = VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT;
        info.srcStage = VK_PIPELINE_STAGE_TRANSFER_BIT;
        info.dstStage = VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT;
    }

    // DEPTH_STENCIL_ATTACHMENT -> TRANSFER_SRC
    else if (oldLayout == VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL &&
             newLayout == VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL) {
        info.srcAccessMask = VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT;
        info.dstAccessMask = VK_ACCESS_TRANSFER_READ_BIT;
        info.srcStage = VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT;
        info.dstStage = VK_PIPELINE_STAGE_TRANSFER_BIT;
    }

    else {
        fprintf(stderr, "Unsupported layout transition: %d -> %d\n",
                (int)oldLayout, (int)newLayout);
        assert(!"Unsupported layout transition");
    }

    return info;
}

static int create_image(image_res_t* out, uint32_t width, uint32_t height,
                        VkFormat format, VkImageUsageFlags usage,
                        VkImageAspectFlags aspect,
                        VkMemoryPropertyFlags memory_properties) {
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
        .samples = VK_SAMPLE_COUNT_1_BIT,
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
    out->current_layout = VK_IMAGE_LAYOUT_UNDEFINED;
    out->extent = (VkExtent2D){.width = width, .height = height};

    return 1;
}

int create_color_image(image_res_t* out, uint32_t width, uint32_t height,
                       VkImageUsageFlags usage,
                       VkMemoryPropertyFlags memory_properties) {
    return create_image(out, width, height, VK_FORMAT_R16G16B16A16_SFLOAT,
                        VK_IMAGE_ASPECT_COLOR_BIT, usage, memory_properties);
}

int create_depth_image(image_res_t* out, uint32_t width, uint32_t height,
                       VkImageUsageFlags usage,
                       VkMemoryPropertyFlags memory_properties) {
    return create_image(out, width, height, VK_FORMAT_D32_SFLOAT,
                        VK_IMAGE_ASPECT_DEPTH_BIT, usage, memory_properties);
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

void transition_image_layout(image_res_t* img, VkImageLayout new_layout,
                             VkCommandBuffer cmd) {
    if (!img || img->image == VK_NULL_HANDLE) return;
    if (img->current_layout == new_layout) return;

    const VkImageLayout old_layout = img->current_layout;
    const TransitionInfo t = get_transition_info(old_layout, new_layout);

    VkImageMemoryBarrier barrier = {
        .sType = VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER,
        .pNext = NULL,
        .srcAccessMask = t.srcAccessMask,
        .dstAccessMask = t.dstAccessMask,
        .oldLayout = old_layout,
        .newLayout = new_layout,
        .srcQueueFamilyIndex = VK_QUEUE_FAMILY_IGNORED,
        .dstQueueFamilyIndex = VK_QUEUE_FAMILY_IGNORED,
        .image = img->image,
        .subresourceRange =
            (VkImageSubresourceRange){
                .aspectMask = img->aspect,
                .baseMipLevel = 0,
                .levelCount = 1,
                .baseArrayLayer = 0,
                .layerCount = 1,
            },
    };

    vkCmdPipelineBarrier(cmd, t.srcStage, t.dstStage, 0, 0, NULL, 0, NULL, 1,
                         &barrier);

    img->current_layout = new_layout;
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
    destroy_gpu_image((image_res_t*)obj);
}

int nif_init_image_res(ErlNifEnv* env) {
    IMAGE_RES_TYPE =
        enif_open_resource_type(env, "fluo_nif", "image_res", image_res_dtor,
                                ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL);

    if (!IMAGE_RES_TYPE) return -1;

    return 0;
}

image_res_t* get_image_from_term(ErlNifEnv* env, ERL_NIF_TERM term) {
    if (!env) return NULL;

    image_res_t* res = NULL;

    if (enif_get_resource(env, term, IMAGE_RES_TYPE, (void**)&res)) {
        return res;
    }

    const ERL_NIF_TERM* elems = NULL;
    int arity = 0;
    if (!enif_get_tuple(env, term, &arity, &elems)) return NULL;
    if (arity <= 0) return NULL;

    ERL_NIF_TERM handle_term = elems[arity - 1];
    if (!enif_get_resource(env, handle_term, IMAGE_RES_TYPE, (void**)&res))
        return NULL;

    return res;
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

    VkImageUsageFlags usage = VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT |
                              VK_IMAGE_USAGE_TRANSFER_SRC_BIT |
                              VK_IMAGE_USAGE_TRANSFER_DST_BIT;

    if (!create_depth_image(img, width, height, usage, 0)) {
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

    VkImageUsageFlags usage = VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT |
                              VK_IMAGE_USAGE_TRANSFER_SRC_BIT |
                              VK_IMAGE_USAGE_TRANSFER_DST_BIT;

    if (!create_color_image(img, width, height, usage, 0)) {
        enif_release_resource(img);
        return enif_make_atom(env, "error");
    }

    ERL_NIF_TERM term = enif_make_resource(env, img);
    enif_release_resource(img);
    return term;
}
