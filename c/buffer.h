#pragma once

#include <vulkan/vulkan.h>
#include "vk_mem_alloc.h"

int create_gpu_buffer(VkDeviceSize size,
                      VkBufferUsageFlags usage,
                      VmaMemoryUsage mem_usage,
                      VkBuffer* out_buf,
                      VmaAllocation* out_alloc);

int write_gpu_buffer(const void* src_data,
                     VkDeviceSize size,
                     VkBufferUsageFlags dst_usage,
                     VkPipelineStageFlags dst_stage,
                     VkAccessFlags dst_access,
                     VkBuffer* out_buf,
                     VmaAllocation* out_alloc);
