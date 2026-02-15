#include <assert.h>

#include "image.h"

typedef struct {
    VkPipelineStageFlags2 srcStage;
    VkPipelineStageFlags2 dstStage;
    VkAccessFlags2 srcAccess;
    VkAccessFlags2 dstAccess;
} TransitionInfo2;

static TransitionInfo2 get_transition_info2(VkImageLayout old,
                                            VkImageLayout new) {
    TransitionInfo2 t = {0};

    // PRESENT -> TRANSFER_DST
    if (old == VK_IMAGE_LAYOUT_PRESENT_SRC_KHR &&
        new == VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL) {
        t.srcStage = VK_PIPELINE_STAGE_2_NONE;
        t.srcAccess = 0;
        t.dstStage = VK_PIPELINE_STAGE_2_TRANSFER_BIT;
        t.dstAccess = VK_ACCESS_2_TRANSFER_WRITE_BIT;
        return t;
    }

    // TRANSFER_DST -> PRESENT
    if (old == VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL &&
        new == VK_IMAGE_LAYOUT_PRESENT_SRC_KHR) {
        t.srcStage = VK_PIPELINE_STAGE_2_TRANSFER_BIT;
        t.srcAccess = VK_ACCESS_2_TRANSFER_WRITE_BIT;
        t.dstStage = VK_PIPELINE_STAGE_2_NONE;
        t.dstAccess = 0;
        return t;
    }

    // COLOR_ATTACHMENT -> TRANSFER_SRC
    if (old == VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL &&
        new == VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL) {
        t.srcStage = VK_PIPELINE_STAGE_2_COLOR_ATTACHMENT_OUTPUT_BIT;
        t.srcAccess = VK_ACCESS_2_COLOR_ATTACHMENT_WRITE_BIT;
        t.dstStage = VK_PIPELINE_STAGE_2_TRANSFER_BIT;
        t.dstAccess = VK_ACCESS_2_TRANSFER_READ_BIT;
        return t;
    }

    // TRANSFER_SRC -> COLOR_ATTACHMENT
    if (old == VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL &&
        new == VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL) {
        t.srcStage = VK_PIPELINE_STAGE_2_TRANSFER_BIT;
        t.srcAccess = VK_ACCESS_2_TRANSFER_READ_BIT;
        t.dstStage = VK_PIPELINE_STAGE_2_COLOR_ATTACHMENT_OUTPUT_BIT;
        t.dstAccess = VK_ACCESS_2_COLOR_ATTACHMENT_WRITE_BIT;
        return t;
    }

    // UNDEFINED -> COLOR_ATTACHMENT
    if (old == VK_IMAGE_LAYOUT_UNDEFINED &&
        new == VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL) {
        t.srcStage = VK_PIPELINE_STAGE_2_NONE;
        t.srcAccess = 0;
        t.dstStage = VK_PIPELINE_STAGE_2_COLOR_ATTACHMENT_OUTPUT_BIT;
        t.dstAccess = VK_ACCESS_2_COLOR_ATTACHMENT_WRITE_BIT;
        return t;
    }

    // UNDEFINED -> DEPTH_STENCIL_ATTACHMENT
    if (old == VK_IMAGE_LAYOUT_UNDEFINED &&
        new == VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL) {
        t.srcStage = VK_PIPELINE_STAGE_2_NONE;
        t.srcAccess = 0;
        t.dstStage = VK_PIPELINE_STAGE_2_EARLY_FRAGMENT_TESTS_BIT |
                     VK_PIPELINE_STAGE_2_LATE_FRAGMENT_TESTS_BIT;
        t.dstAccess = VK_ACCESS_2_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT;
        return t;
    }

    // DEPTH_STENCIL_ATTACHMENT -> TRANSFER_SRC
    if (old == VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL &&
        new == VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL) {
        t.srcStage = VK_PIPELINE_STAGE_2_EARLY_FRAGMENT_TESTS_BIT |
                     VK_PIPELINE_STAGE_2_LATE_FRAGMENT_TESTS_BIT;
        t.srcAccess = VK_ACCESS_2_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT;
        t.dstStage = VK_PIPELINE_STAGE_2_TRANSFER_BIT;
        t.dstAccess = VK_ACCESS_2_TRANSFER_READ_BIT;
        return t;
    }

    // UNDEFINED -> TRANSFER_DST
    if (old == VK_IMAGE_LAYOUT_UNDEFINED &&
        new == VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL) {
        t.srcStage = VK_PIPELINE_STAGE_2_NONE;
        t.srcAccess = 0;
        t.dstStage = VK_PIPELINE_STAGE_2_TRANSFER_BIT;
        t.dstAccess = VK_ACCESS_2_TRANSFER_WRITE_BIT;
        return t;
    }

    // TRANSFER_DST -> SHADER_READ_ONLY
    if (old == VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL &&
        new == VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL) {
        t.srcStage = VK_PIPELINE_STAGE_2_TRANSFER_BIT;
        t.srcAccess = VK_ACCESS_2_TRANSFER_WRITE_BIT;
        t.dstStage = VK_PIPELINE_STAGE_2_FRAGMENT_SHADER_BIT;
        t.dstAccess = VK_ACCESS_2_SHADER_SAMPLED_READ_BIT;
        return t;
    }

    // SHADER_READ_ONLY -> TRANSFER_SRC
    if (old == VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL &&
        new == VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL) {
        t.srcStage = VK_PIPELINE_STAGE_2_FRAGMENT_SHADER_BIT;
        t.srcAccess = VK_ACCESS_2_SHADER_SAMPLED_READ_BIT;
        t.dstStage = VK_PIPELINE_STAGE_2_TRANSFER_BIT;
        t.dstAccess = VK_ACCESS_2_TRANSFER_READ_BIT;
        return t;
    }

    // TRANSFER_SRC -> SHADER_READ_ONLY
    else if (old == VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL &&
             new == VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL) {
        t.srcStage = VK_PIPELINE_STAGE_2_TRANSFER_BIT;
        t.srcAccess = VK_ACCESS_2_TRANSFER_READ_BIT;
        t.dstStage = VK_PIPELINE_STAGE_2_FRAGMENT_SHADER_BIT;
        t.dstAccess = VK_ACCESS_2_SHADER_SAMPLED_READ_BIT;
        return t;
    }

    // TRANSFER_DST -> TRANSFER_SRC
    if (old == VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL &&
        new == VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL) {
        t.srcStage = VK_PIPELINE_STAGE_2_TRANSFER_BIT;
        t.srcAccess = VK_ACCESS_2_TRANSFER_WRITE_BIT;
        t.dstStage = VK_PIPELINE_STAGE_2_TRANSFER_BIT;
        t.dstAccess = VK_ACCESS_2_TRANSFER_READ_BIT;
        return t;
    }

    // TRANSFER_SRC -> TRANSFER_DST
    if (old == VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL &&
        new == VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL) {
        t.srcStage = VK_PIPELINE_STAGE_2_TRANSFER_BIT;
        t.srcAccess = VK_ACCESS_2_TRANSFER_READ_BIT;
        t.dstStage = VK_PIPELINE_STAGE_2_TRANSFER_BIT;
        t.dstAccess = VK_ACCESS_2_TRANSFER_WRITE_BIT;
        return t;
    }

    fprintf(stderr, "Unsupported layout transition2: %d -> %d\n", (int)old,
            (int)new);

    assert(!"Unsupported layout transition2");

    return t;
}

void transition_image_layout(image_res_t* img, VkImageLayout new_layout,
                             VkCommandBuffer cmd) {
    if (!img || img->image == VK_NULL_HANDLE) return;
    if (img->current_layout == new_layout) return;

    VkImageLayout old_layout = img->current_layout;
    TransitionInfo2 t = get_transition_info2(old_layout, new_layout);

    VkImageMemoryBarrier2 barrier = {
        .sType = VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER_2,
        .srcStageMask = t.srcStage,
        .srcAccessMask = t.srcAccess,
        .dstStageMask = t.dstStage,
        .dstAccessMask = t.dstAccess,
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

    VkDependencyInfo dep = {
        .sType = VK_STRUCTURE_TYPE_DEPENDENCY_INFO,
        .imageMemoryBarrierCount = 1,
        .pImageMemoryBarriers = &barrier,
    };

    vkCmdPipelineBarrier2(cmd, &dep);

    img->current_layout = new_layout;
}

void transition_image_to_optimal_layout(image_res_t* img, VkCommandBuffer cmd) {
    if (!img || img->image == VK_NULL_HANDLE) return;

    transition_image_layout(img, img->optimal_layout, cmd);
}
