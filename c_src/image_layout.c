#include <assert.h>

#include "image.h"

typedef struct {
    VkAccessFlags srcAccessMask;
    VkAccessFlags dstAccessMask;
    VkPipelineStageFlags srcStage;
    VkPipelineStageFlags dstStage;
} TransitionInfo;

TransitionInfo get_transition_info(VkImageLayout old, VkImageLayout newLayout) {
    TransitionInfo info = {0};

    // UNDEFINED -> ANY (you were using TOP->TOP)
    if (old == VK_IMAGE_LAYOUT_UNDEFINED) {
        info.srcAccessMask = 0;
        info.dstAccessMask = 0;
        info.srcStage = VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT;
        info.dstStage = VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT;
    }

    // PRESENT -> TRANSFER_DST
    else if (old == VK_IMAGE_LAYOUT_PRESENT_SRC_KHR &&
             newLayout == VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL) {
        info.srcAccessMask = 0;
        info.dstAccessMask = VK_ACCESS_TRANSFER_WRITE_BIT;
        info.srcStage = VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT;
        info.dstStage = VK_PIPELINE_STAGE_TRANSFER_BIT;
    }

    // TRANSFER_DST -> PRESENT
    else if (old == VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL &&
             newLayout == VK_IMAGE_LAYOUT_PRESENT_SRC_KHR) {
        info.srcAccessMask = VK_ACCESS_TRANSFER_WRITE_BIT;
        info.dstAccessMask = 0;
        info.srcStage = VK_PIPELINE_STAGE_TRANSFER_BIT;
        info.dstStage = VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT;
    }

    // TRANSFER_SRC -> COLOR_ATTACHMENT
    else if (old == VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL &&
             newLayout == VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL) {
        info.srcAccessMask = VK_ACCESS_TRANSFER_READ_BIT;
        info.dstAccessMask = VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT;
        info.srcStage = VK_PIPELINE_STAGE_TRANSFER_BIT;
        info.dstStage = VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT;
    }

    // GENERAL -> ANY (you were using TOP->TOP)
    else if (old == VK_IMAGE_LAYOUT_GENERAL) {
        info.srcAccessMask = 0;
        info.dstAccessMask = 0;
        info.srcStage = VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT;
        info.dstStage = VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT;
    }

    // TRANSFER_DST -> COLOR_ATTACHMENT
    else if (old == VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL &&
             newLayout == VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL) {
        info.srcAccessMask = VK_ACCESS_TRANSFER_WRITE_BIT;
        info.dstAccessMask = VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT;
        info.srcStage = VK_PIPELINE_STAGE_TRANSFER_BIT;
        info.dstStage = VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT;
    }

    // COLOR_ATTACHMENT -> TRANSFER_DST
    else if (old == VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL &&
             newLayout == VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL) {
        info.srcAccessMask = VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT;
        info.dstAccessMask = VK_ACCESS_TRANSFER_WRITE_BIT;
        info.srcStage = VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT;
        info.dstStage = VK_PIPELINE_STAGE_TRANSFER_BIT;
    }

    // COLOR_ATTACHMENT -> TRANSFER_SRC
    else if (old == VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL &&
             newLayout == VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL) {
        info.srcAccessMask = VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT;
        info.dstAccessMask = VK_ACCESS_TRANSFER_READ_BIT;
        info.srcStage = VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT;
        info.dstStage = VK_PIPELINE_STAGE_TRANSFER_BIT;
    }

    // TRANSFER_DST -> DEPTH_STENCIL_ATTACHMENT
    else if (old == VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL &&
             newLayout == VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL) {
        info.srcAccessMask = VK_ACCESS_TRANSFER_WRITE_BIT;
        info.dstAccessMask = VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT;
        info.srcStage = VK_PIPELINE_STAGE_TRANSFER_BIT;
        info.dstStage = VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT;
    }

    // DEPTH_STENCIL_ATTACHMENT -> TRANSFER_SRC
    else if (old == VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL &&
             newLayout == VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL) {
        info.srcAccessMask = VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT;
        info.dstAccessMask = VK_ACCESS_TRANSFER_READ_BIT;
        info.srcStage = VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT;
        info.dstStage = VK_PIPELINE_STAGE_TRANSFER_BIT;
    }

    // UNDEFINED -> TRANSFER_DST
    else if (old == VK_IMAGE_LAYOUT_UNDEFINED &&
             newLayout == VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL) {
        info.srcAccessMask = 0;
        info.dstAccessMask = VK_ACCESS_TRANSFER_WRITE_BIT;
        info.srcStage = VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT;
        info.dstStage = VK_PIPELINE_STAGE_TRANSFER_BIT;
    }

    // TRANSFER_DST -> SHADER_READ_ONLY
    else if (old == VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL &&
             newLayout == VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL) {
        info.srcAccessMask = VK_ACCESS_TRANSFER_WRITE_BIT;
        info.dstAccessMask = VK_ACCESS_SHADER_READ_BIT;
        info.srcStage = VK_PIPELINE_STAGE_TRANSFER_BIT;
        info.dstStage = VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT;
    }

    // SHADER_ONLY -> ATTACHMENT_OPTIMAL
    else if (old == VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL &&
             newLayout == VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL) {
        info.srcAccessMask = VK_ACCESS_SHADER_READ_BIT;
        info.dstAccessMask = VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT;
        info.srcStage = VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT;
        info.dstStage = VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT;
    }

    // ATTACHMENT_OPTIMAL -> SHADER_ONLY
    else if (old == VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL &&
             newLayout == VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL) {
        info.srcAccessMask = VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT;
        info.dstAccessMask = VK_ACCESS_SHADER_READ_BIT;
        info.srcStage = VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT;
        info.dstStage = VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT;
    }

    else {
        fprintf(stderr, "Unsupported layout transition: %d -> %d\n", (int)old,
                (int)newLayout);
        assert(!"Unsupported layout transition");
    }

    return info;
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
