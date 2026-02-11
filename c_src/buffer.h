#pragma once

#include <vulkan/vulkan.h>
#include "vk_mem_alloc.h"

typedef struct {
    VkBuffer buffer;
    VmaAllocation alloc;
    VmaAllocationInfo info;
    void* mapped;
    VkDeviceSize size;
    VkBufferUsageFlags usage;
    VkMemoryPropertyFlags memory_properties;
} GpuBuffer;

int create_gpu_buffer(GpuBuffer* out,
                      VkDeviceSize size,
                      VkBufferUsageFlags usage,
                      VkMemoryPropertyFlags memoryProperties);

void destroy_gpu_buffer(GpuBuffer* buf);

int write_gpu_buffer(GpuBuffer* buf,
                     const void* src,
                     VkDeviceSize size,
                     VkDeviceSize offset,
                     VkPipelineStageFlags dst_stage,
                     VkAccessFlags dst_access);

int direct_write_gpu_buffer(GpuBuffer* buf, const void* src, VkDeviceSize size, VkDeviceSize offset);

int read_gpu_buffer(GpuBuffer* buf, void* dst, VkDeviceSize size, VkDeviceSize offset);
