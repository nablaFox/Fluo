#pragma once

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <vulkan/vulkan.h>

#include "vk_mem_alloc.h"

struct Device {
    VkInstance instance;
    VkPhysicalDevice physical_device;
    VkDevice logical_device;
    VkQueue graphics_queue;
    VkQueue present_queue;
    uint32_t graphics_family;
    uint32_t present_family;
    VmaAllocator allocator;
#ifdef DEBUG
    VkDebugUtilsMessengerEXT debug_messenger;
#endif
};

extern struct Device g_device;

void init_device();
void destroy_device();
