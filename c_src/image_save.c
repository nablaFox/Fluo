#include "device.h"
#include "erl_nif.h"
#include "image.h"

#define STB_IMAGE_WRITE_IMPLEMENTATION
#include "stb_image_write.h"

static void transition_image_layout_raw(VkCommandBuffer cmd, VkImage image,
                                        VkImageAspectFlags aspect,
                                        VkImageLayout old_layout,
                                        VkImageLayout new_layout) {
    if (old_layout == new_layout) return;

    VkImageMemoryBarrier2 barrier = {
        .sType = VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER_2,
        .pNext = NULL,

        .srcStageMask = VK_PIPELINE_STAGE_2_ALL_COMMANDS_BIT,
        .srcAccessMask =
            VK_ACCESS_2_MEMORY_WRITE_BIT | VK_ACCESS_2_MEMORY_READ_BIT,
        .dstStageMask = VK_PIPELINE_STAGE_2_ALL_COMMANDS_BIT,
        .dstAccessMask =
            VK_ACCESS_2_MEMORY_WRITE_BIT | VK_ACCESS_2_MEMORY_READ_BIT,

        .oldLayout = old_layout,
        .newLayout = new_layout,

        .srcQueueFamilyIndex = VK_QUEUE_FAMILY_IGNORED,
        .dstQueueFamilyIndex = VK_QUEUE_FAMILY_IGNORED,

        .image = image,
        .subresourceRange =
            {
                .aspectMask = aspect,
                .baseMipLevel = 0,
                .levelCount = 1,
                .baseArrayLayer = 0,
                .layerCount = 1,
            },
    };

    VkDependencyInfo dep = {
        .sType = VK_STRUCTURE_TYPE_DEPENDENCY_INFO,
        .pNext = NULL,
        .dependencyFlags = 0,
        .memoryBarrierCount = 0,
        .pMemoryBarriers = NULL,
        .bufferMemoryBarrierCount = 0,
        .pBufferMemoryBarriers = NULL,
        .imageMemoryBarrierCount = 1,
        .pImageMemoryBarriers = &barrier,
    };

    vkCmdPipelineBarrier2(cmd, &dep);
}

static int get_path_from_term(ErlNifEnv* env, ERL_NIF_TERM term, char* out,
                              size_t out_sz) {
    ErlNifBinary bin;

    if (!enif_inspect_iolist_as_binary(env, term, &bin)) {
        return 0;
    }

    if (bin.size + 1 > out_sz) {
        return 0;
    }

    memcpy(out, bin.data, bin.size);
    out[bin.size] = '\0';
    return 1;
}

ERL_NIF_TERM nif_save_color_image_to_png(ErlNifEnv* env, int argc,
                                         const ERL_NIF_TERM argv[]) {
    if (argc != 2) return enif_make_badarg(env);

    image_res_t* img = get_image_from_term(env, argv[0]);
    if (!img) return enif_make_badarg(env);

    if (img->format != VK_FORMAT_R8G8B8A8_UNORM &&
        img->format != VK_FORMAT_B8G8R8A8_UNORM) {
        return enif_make_badarg(env);
    }

    if ((img->aspect & VK_IMAGE_ASPECT_COLOR_BIT) == 0) {
        return enif_make_badarg(env);
    }

    const uint32_t w = img->extent.width;
    const uint32_t h = img->extent.height;
    if (w == 0 || h == 0) return enif_make_badarg(env);

    char path[4096];
    if (!get_path_from_term(env, argv[1], path, sizeof(path))) {
        fprintf(
            stderr,
            "Failed to get path from arg (expected iolist/binary/string)\n");
        return enif_make_badarg(env);
    }

    const VkDeviceSize bpp = 4;
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
        .flags = VMA_ALLOCATION_CREATE_HOST_ACCESS_RANDOM_BIT,
        .usage = VMA_MEMORY_USAGE_AUTO,
    };

    if (vmaCreateBuffer(g_device.allocator, &buf_info, &alloc_info,
                        &staging_buf, &staging_alloc, NULL) != VK_SUCCESS) {
        return enif_make_atom(env, "error");
    }

    VkImage resolve_image = VK_NULL_HANDLE;
    VmaAllocation resolve_alloc = VK_NULL_HANDLE;
    VkImageLayout resolve_layout = VK_IMAGE_LAYOUT_UNDEFINED;

    VkImage copy_src_image = img->image;

    VkCommandBuffer cmd = begin_single_time_commands();

    if (img->samples != VK_SAMPLE_COUNT_1_BIT) {
        VkImageCreateInfo resolve_info = {
            .sType = VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO,
            .imageType = VK_IMAGE_TYPE_2D,
            .format = img->format,
            .extent = {w, h, 1},
            .mipLevels = 1,
            .arrayLayers = 1,
            .samples = VK_SAMPLE_COUNT_1_BIT,
            .tiling = VK_IMAGE_TILING_OPTIMAL,
            .usage = VK_IMAGE_USAGE_TRANSFER_DST_BIT |
                     VK_IMAGE_USAGE_TRANSFER_SRC_BIT,
            .sharingMode = VK_SHARING_MODE_EXCLUSIVE,
            .initialLayout = VK_IMAGE_LAYOUT_UNDEFINED,
        };

        VmaAllocationCreateInfo resolve_alloc_info = {
            .usage = VMA_MEMORY_USAGE_AUTO,
        };

        if (vmaCreateImage(g_device.allocator, &resolve_info,
                           &resolve_alloc_info, &resolve_image, &resolve_alloc,
                           NULL) != VK_SUCCESS) {
            end_single_time_commands(cmd);
            vmaDestroyBuffer(g_device.allocator, staging_buf, staging_alloc);
            return enif_make_atom(env, "error");
        }

        VkImageLayout old_layout = img->current_layout;
        transition_image_layout(img, VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL, cmd);

        transition_image_layout_raw(cmd, resolve_image,
                                    VK_IMAGE_ASPECT_COLOR_BIT, resolve_layout,
                                    VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL);
        resolve_layout = VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL;

        VkImageResolve region = {
            .srcSubresource = {VK_IMAGE_ASPECT_COLOR_BIT, 0, 0, 1},
            .srcOffset = {0, 0, 0},
            .dstSubresource = {VK_IMAGE_ASPECT_COLOR_BIT, 0, 0, 1},
            .dstOffset = {0, 0, 0},
            .extent = {w, h, 1},
        };

        vkCmdResolveImage(cmd, img->image, VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
                          resolve_image, VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
                          1, &region);

        copy_src_image = resolve_image;

        transition_image_layout_raw(cmd, resolve_image,
                                    VK_IMAGE_ASPECT_COLOR_BIT, resolve_layout,
                                    VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL);

        resolve_layout = VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL;

        transition_image_layout(img, old_layout, cmd);
    }

    VkImageLayout old_layout_for_copy = VK_IMAGE_LAYOUT_UNDEFINED;
    int needs_restore = 0;

    if (img->samples == VK_SAMPLE_COUNT_1_BIT) {
        old_layout_for_copy = img->current_layout;
        needs_restore = 1;
        transition_image_layout(img, VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL, cmd);
    }

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
        .imageExtent = {w, h, 1},
    };

    vkCmdCopyImageToBuffer(cmd, copy_src_image,
                           VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL, staging_buf, 1,
                           &region);

    if (needs_restore) {
        transition_image_layout(img, old_layout_for_copy, cmd);
    }

    if (!end_single_time_commands(cmd)) {
        if (resolve_image != VK_NULL_HANDLE)
            vmaDestroyImage(g_device.allocator, resolve_image, resolve_alloc);
        vmaDestroyBuffer(g_device.allocator, staging_buf, staging_alloc);
        return enif_make_atom(env, "error");
    }

    void* mapped = NULL;
    if (vmaMapMemory(g_device.allocator, staging_alloc, &mapped) !=
            VK_SUCCESS ||
        !mapped) {
        if (resolve_image != VK_NULL_HANDLE)
            vmaDestroyImage(g_device.allocator, resolve_image, resolve_alloc);
        vmaDestroyBuffer(g_device.allocator, staging_buf, staging_alloc);
        return enif_make_atom(env, "error");
    }

    vmaInvalidateAllocation(g_device.allocator, staging_alloc, 0, size);

    const int stride = (int)(w * 4);
    int ok = stbi_write_png(path, (int)w, (int)h, 4, mapped, stride);

    vmaUnmapMemory(g_device.allocator, staging_alloc);

    if (resolve_image != VK_NULL_HANDLE)
        vmaDestroyImage(g_device.allocator, resolve_image, resolve_alloc);

    vmaDestroyBuffer(g_device.allocator, staging_buf, staging_alloc);

    if (!ok) return enif_make_atom(env, "error");
    return enif_make_atom(env, "ok");
}
