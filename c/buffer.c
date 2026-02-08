#include "buffer.h"

#include <string.h>

#include "device.h"

static VkCommandBuffer begin_single_time_commands(void) {
    VkCommandBufferAllocateInfo alloc = {
        .sType = VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO,
        .commandPool = g_device.upload_cmd_pool,
        .level = VK_COMMAND_BUFFER_LEVEL_PRIMARY,
        .commandBufferCount = 1,
    };

    VkCommandBuffer cmd;
    vkAllocateCommandBuffers(g_device.logical_device, &alloc, &cmd);

    VkCommandBufferBeginInfo begin = {
        .sType = VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO,
        .flags = VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT,
    };
    vkBeginCommandBuffer(cmd, &begin);
    return cmd;
}

static int end_single_time_commands(VkCommandBuffer cmd) {
    vkEndCommandBuffer(cmd);

    VkSubmitInfo submit = {
        .sType = VK_STRUCTURE_TYPE_SUBMIT_INFO,
        .commandBufferCount = 1,
        .pCommandBuffers = &cmd,
    };

    VkFenceCreateInfo fence_info = {.sType =
                                        VK_STRUCTURE_TYPE_FENCE_CREATE_INFO};
    VkFence fence;
    vkCreateFence(g_device.logical_device, &fence_info, NULL, &fence);

    VkResult r = vkQueueSubmit(g_device.graphics_queue, 1, &submit, fence);
    if (r != VK_SUCCESS) return 0;

    r = vkWaitForFences(g_device.logical_device, 1, &fence, VK_TRUE,
                        UINT64_MAX);
    vkDestroyFence(g_device.logical_device, fence, NULL);

    vkFreeCommandBuffers(g_device.logical_device, g_device.upload_cmd_pool, 1,
                         &cmd);
    return r == VK_SUCCESS;
}

int create_gpu_buffer(VkDeviceSize size, VkBufferUsageFlags usage,
                      VmaMemoryUsage mem_usage, VkBuffer* out_buf,
                      VmaAllocation* out_alloc) {
    VkBufferCreateInfo buf_info = {
        .sType = VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO,
        .size = size,
        .usage = usage,
        .sharingMode = VK_SHARING_MODE_EXCLUSIVE,
    };

    VmaAllocationCreateInfo alloc_info = {
        .usage = mem_usage,
    };

    return vmaCreateBuffer(g_device.allocator, &buf_info, &alloc_info, out_buf,
                           out_alloc, NULL) == VK_SUCCESS;
}
static int create_staging_buffer(VkDeviceSize size, VkBuffer* out_buf,
                                 VmaAllocation* out_alloc, void** out_mapped) {
    VkBufferCreateInfo buf_info = {
        .sType = VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO,
        .size = size,
        .usage = VK_BUFFER_USAGE_TRANSFER_SRC_BIT,
        .sharingMode = VK_SHARING_MODE_EXCLUSIVE,
    };

    VmaAllocationCreateInfo alloc_info = {
        .usage = VMA_MEMORY_USAGE_CPU_ONLY,
        .flags = VMA_ALLOCATION_CREATE_MAPPED_BIT,
    };

    VmaAllocationInfo info;
    if (vmaCreateBuffer(g_device.allocator, &buf_info, &alloc_info, out_buf,
                        out_alloc, &info) != VK_SUCCESS)
        return 0;

    *out_mapped = info.pMappedData;
    return 1;
}

int write_gpu_buffer(const void* src_data, VkDeviceSize size,
                     VkBufferUsageFlags dst_usage,
                     VkPipelineStageFlags dst_stage, VkAccessFlags dst_access,
                     VkBuffer* out_buf, VmaAllocation* out_alloc) {
    if (!src_data || size == 0 || !out_buf || !out_alloc) return 0;

    VkBuffer dst_buf = VK_NULL_HANDLE;
    VmaAllocation dst_alloc = NULL;

    if (!create_gpu_buffer(size, dst_usage | VK_BUFFER_USAGE_TRANSFER_DST_BIT,
                           VMA_MEMORY_USAGE_GPU_ONLY, &dst_buf, &dst_alloc)) {
        return 0;
    }

    VkBuffer stage_buf = VK_NULL_HANDLE;
    VmaAllocation stage_alloc = NULL;
    void* mapped = NULL;

    if (!create_staging_buffer(size, &stage_buf, &stage_alloc, &mapped)) {
        vmaDestroyBuffer(g_device.allocator, dst_buf, dst_alloc);
        return 0;
    }

    memcpy(mapped, src_data, (size_t)size);

    VkCommandBuffer cmd = begin_single_time_commands();

    VkBufferCopy copy = {.srcOffset = 0, .dstOffset = 0, .size = size};
    vkCmdCopyBuffer(cmd, stage_buf, dst_buf, 1, &copy);

    VkBufferMemoryBarrier barrier = {
        .sType = VK_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER,
        .srcAccessMask = VK_ACCESS_TRANSFER_WRITE_BIT,
        .dstAccessMask = dst_access,
        .srcQueueFamilyIndex = VK_QUEUE_FAMILY_IGNORED,
        .dstQueueFamilyIndex = VK_QUEUE_FAMILY_IGNORED,
        .buffer = dst_buf,
        .offset = 0,
        .size = VK_WHOLE_SIZE,
    };

    vkCmdPipelineBarrier(cmd, VK_PIPELINE_STAGE_TRANSFER_BIT, dst_stage, 0, 0,
                         NULL, 1, &barrier, 0, NULL);

    const int ok = end_single_time_commands(cmd);

    vmaDestroyBuffer(g_device.allocator, stage_buf, stage_alloc);

    if (!ok) {
        vmaDestroyBuffer(g_device.allocator, dst_buf, dst_alloc);
        return 0;
    }

    *out_buf = dst_buf;
    *out_alloc = dst_alloc;
    return 1;
}
