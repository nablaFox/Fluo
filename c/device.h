#pragma once

#include <assert.h>
#include <vulkan/vulkan.h>

#include "vk_mem_alloc.h"

#define MAX_BINDLESS_RESOURCES 1024

#define STORAGE_BUFFER_BINDING 0
#define UNIFORM_BINDING 1

struct Device {
    VkInstance instance;
    VkPhysicalDevice physical_device;
    VkDevice logical_device;
    VkQueue graphics_queue;
    VkQueue present_queue;
    uint32_t graphics_family;
    uint32_t present_family;
    VmaAllocator allocator;

    VkDescriptorSetLayout descriptor_layout;
    VkDescriptorPool descriptor_pool;
    VkDescriptorSet descriptor_set;
    VkPipelineLayout pipeline_layout;

    VkCommandPool upload_cmd_pool;

#ifdef DEBUG
    VkDebugUtilsMessengerEXT debug_messenger;
#endif
};

extern struct Device g_device;

void init_device();
void destroy_device();

VkCommandBuffer begin_single_time_commands(void);
int end_single_time_commands(VkCommandBuffer cmd);
