#include "buffer.h"

#include <string.h>

#include "device.h"

static void gpu_buffer_zero(GpuBuffer* b) {
    *b = (GpuBuffer){
        .buffer = VK_NULL_HANDLE,
        .alloc = NULL,
        .info = {0},
        .size = 0,
        .usage = 0,
        .memory_properties = 0,
    };
}

int create_gpu_buffer(GpuBuffer* out, VkDeviceSize size,
                      VkBufferUsageFlags usage,
                      VkMemoryPropertyFlags memoryProperties) {
    if (!out || size == 0) return 0;

    gpu_buffer_zero(out);

    VkBufferCreateInfo const buff_info = {
        .sType = VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO,
        .size = size,
        .usage = usage,
        .sharingMode = VK_SHARING_MODE_EXCLUSIVE,
    };

    VmaAllocationCreateInfo alloc_info = {
        .requiredFlags = memoryProperties,
    };

    if (memoryProperties & VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT) {
        alloc_info.flags |= VMA_ALLOCATION_CREATE_MAPPED_BIT;
    }

    VkResult vr = vmaCreateBuffer(g_device.allocator, &buff_info, &alloc_info,
                                  &out->buffer, &out->alloc, &out->info);

    if (vr != VK_SUCCESS) {
        gpu_buffer_zero(out);
        return 0;
    }

    out->size = size;
    out->usage = usage;
    out->memory_properties = memoryProperties;

    out->mapped = out->info.pMappedData;

    return 1;
}

void destroy_gpu_buffer(GpuBuffer* buf) {
    if (!buf) return;

    if (buf->buffer != VK_NULL_HANDLE && buf->alloc) {
        vmaDestroyBuffer(g_device.allocator, buf->buffer, buf->alloc);
    }

    gpu_buffer_zero(buf);
}

int direct_write_gpu_buffer(GpuBuffer* buf, const void* src, VkDeviceSize size,
                            VkDeviceSize offset) {
    if (!buf || !src) return 0;
    if (!buf->mapped) return 0;
    if (offset + size > buf->size) return 0;

    memcpy((uint8_t*)buf->mapped + offset, src, (size_t)size);

    if (!(buf->memory_properties & VK_MEMORY_PROPERTY_HOST_COHERENT_BIT)) {
        vmaFlushAllocation(g_device.allocator, buf->alloc, offset, size);
    }

    return 1;
}

int write_gpu_buffer(GpuBuffer* buf, const void* src, VkDeviceSize size,
                     VkDeviceSize offset, VkPipelineStageFlags dst_stage,
                     VkAccessFlags dst_access) {
    if (!buf || !src) return 0;
    if (size == 0) return 0;

    if ((buf->usage & VK_BUFFER_USAGE_TRANSFER_DST_BIT) == 0) return 0;

    if (offset + size > buf->size) return 0;

    GpuBuffer staging;
    if (!create_gpu_buffer(&staging, size, VK_BUFFER_USAGE_TRANSFER_SRC_BIT,
                           VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT |
                               VK_MEMORY_PROPERTY_HOST_COHERENT_BIT)) {
        return 0;
    }

    void* mapped = NULL;

    if (vmaMapMemory(g_device.allocator, staging.alloc, &mapped) !=
        VK_SUCCESS) {
        destroy_gpu_buffer(&staging);
        return 0;
    }

    memcpy(mapped, src, (size_t)size);

    if ((staging.memory_properties & VK_MEMORY_PROPERTY_HOST_COHERENT_BIT) ==
        0) {
        vmaFlushAllocation(g_device.allocator, staging.alloc, 0, size);
    }

    vmaUnmapMemory(g_device.allocator, staging.alloc);

    VkCommandBuffer cmd = begin_single_time_commands();

    VkBufferCopy copy = {
        .srcOffset = 0,
        .dstOffset = offset,
        .size = size,
    };

    vkCmdCopyBuffer(cmd, staging.buffer, buf->buffer, 1, &copy);

    VkBufferMemoryBarrier barrier = {
        .sType = VK_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER,
        .srcAccessMask = VK_ACCESS_TRANSFER_WRITE_BIT,
        .dstAccessMask = dst_access,
        .srcQueueFamilyIndex = VK_QUEUE_FAMILY_IGNORED,
        .dstQueueFamilyIndex = VK_QUEUE_FAMILY_IGNORED,
        .buffer = buf->buffer,
        .offset = offset,
        .size = size,
    };

    vkCmdPipelineBarrier(cmd, VK_PIPELINE_STAGE_TRANSFER_BIT, dst_stage, 0, 0,
                         NULL, 1, &barrier, 0, NULL);

    const int ok = end_single_time_commands(cmd);

    destroy_gpu_buffer(&staging);

    return ok;
}

int read_gpu_buffer(GpuBuffer* buf, void* dst, VkDeviceSize size,
                    VkDeviceSize offset) {
    if (!buf || !dst) return 0;
    if (!(buf->memory_properties & VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT))
        return 0;
    if (size == 0) return 0;
    if (offset + size > buf->size) return 0;

    if ((buf->memory_properties & VK_MEMORY_PROPERTY_HOST_COHERENT_BIT) == 0) {
        vmaInvalidateAllocation(g_device.allocator, buf->alloc, offset, size);
    }

    void* mapped = NULL;
    if (vmaMapMemory(g_device.allocator, buf->alloc, &mapped) != VK_SUCCESS) {
        return 0;
    }

    memcpy(dst, (uint8_t*)mapped + offset, (size_t)size);

    vmaUnmapMemory(g_device.allocator, buf->alloc);
    return 1;
}
