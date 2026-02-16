#include "device.h"

#include <stdlib.h>
#include <string.h>

#include "renderer.h"

#ifdef DEBUG
#include <erl_nif.h>
#endif

#define GLFW_INCLUDE_VULKAN
#include <GLFW/glfw3.h>

struct Device g_device = {0};

#ifdef DEBUG
static VKAPI_ATTR VkBool32 VKAPI_CALL
debug_callback(VkDebugUtilsMessageSeverityFlagBitsEXT severity,
               VkDebugUtilsMessageTypeFlagsEXT type,
               const VkDebugUtilsMessengerCallbackDataEXT* callback_data,
               void* user_data) {
    (void)type;
    (void)user_data;

    const char* prefix = "VERBOSE";
    if (severity & VK_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT)
        prefix = "ERROR";
    else if (severity & VK_DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT)
        prefix = "WARNING";
    else if (severity & VK_DEBUG_UTILS_MESSAGE_SEVERITY_INFO_BIT_EXT)
        prefix = "INFO";

    enif_fprintf(stderr, "[Vulkan %s] %s\n", prefix, callback_data->pMessage);
    return VK_FALSE;
}

static void create_debug_messenger(void) {
    VkDebugUtilsMessengerCreateInfoEXT create_info = {
        .sType = VK_STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CREATE_INFO_EXT,
        .messageSeverity = VK_DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT |
                           VK_DEBUG_UTILS_MESSAGE_SEVERITY_INFO_BIT_EXT |
                           VK_DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT |
                           VK_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT,
        .messageType = VK_DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT |
                       VK_DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT |
                       VK_DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT,
        .pfnUserCallback = debug_callback,
    };

    PFN_vkCreateDebugUtilsMessengerEXT func =
        (PFN_vkCreateDebugUtilsMessengerEXT)vkGetInstanceProcAddr(
            g_device.instance, "vkCreateDebugUtilsMessengerEXT");

    assert(func != NULL && "failed to load vkCreateDebugUtilsMessengerEXT");

    VkResult result =
        func(g_device.instance, &create_info, NULL, &g_device.debug_messenger);
    assert(result == VK_SUCCESS && "failed to create debug messenger");
}

static void destroy_debug_messenger(void) {
    PFN_vkDestroyDebugUtilsMessengerEXT func =
        (PFN_vkDestroyDebugUtilsMessengerEXT)vkGetInstanceProcAddr(
            g_device.instance, "vkDestroyDebugUtilsMessengerEXT");

    if (func != NULL) {
        func(g_device.instance, g_device.debug_messenger, NULL);
    }
}
#endif

static int find_queue_families(VkPhysicalDevice device, VkSurfaceKHR surface) {
    uint32_t count = 0;
    vkGetPhysicalDeviceQueueFamilyProperties(device, &count, NULL);

    VkQueueFamilyProperties* families = (VkQueueFamilyProperties*)malloc(
        count * sizeof(VkQueueFamilyProperties));

    vkGetPhysicalDeviceQueueFamilyProperties(device, &count, families);

    int found_graphics = 0;
    int found_present = 0;

    for (uint32_t i = 0; i < count; i++) {
        if (families[i].queueFlags & VK_QUEUE_GRAPHICS_BIT) {
            g_device.graphics_family = i;
            found_graphics = 1;
        }

        if (surface != VK_NULL_HANDLE) {
            VkBool32 present_support = VK_FALSE;
            vkGetPhysicalDeviceSurfaceSupportKHR(device, i, surface,
                                                 &present_support);
            if (present_support) {
                g_device.present_family = i;
                found_present = 1;
            }
        } else {
            g_device.present_family = g_device.graphics_family;
            found_present = found_graphics;
        }

        if (found_graphics && found_present) break;
    }

    free(families);
    return found_graphics && found_present;
}

static int check_device_extension_support(VkPhysicalDevice device,
                                          const char** required,
                                          uint32_t required_count) {
    uint32_t count = 0;
    vkEnumerateDeviceExtensionProperties(device, NULL, &count, NULL);

    VkExtensionProperties* available =
        (VkExtensionProperties*)malloc(count * sizeof(VkExtensionProperties));

    vkEnumerateDeviceExtensionProperties(device, NULL, &count, available);

    for (uint32_t i = 0; i < required_count; i++) {
        int found = 0;
        for (uint32_t j = 0; j < count; j++) {
            if (strcmp(required[i], available[j].extensionName) == 0) {
                found = 1;
                break;
            }
        }
        if (!found) {
            free(available);
            return 0;
        }
    }

    free(available);
    return 1;
}

static void create_instance(void) {
    if (!glfwInit()) {
        assert(0 && "failed to initialize GLFW");
    }

    VkApplicationInfo app_info = {
        .sType = VK_STRUCTURE_TYPE_APPLICATION_INFO,
        .pApplicationName = "VulkanApp",
        .applicationVersion = VK_MAKE_VERSION(1, 0, 0),
        .pEngineName = "None",
        .engineVersion = VK_MAKE_VERSION(1, 0, 0),
        .apiVersion = VK_API_VERSION_1_3,
    };

    uint32_t glfw_ext_count = 0;
    const char** glfw_extensions =
        glfwGetRequiredInstanceExtensions(&glfw_ext_count);

#ifdef DEBUG
    uint32_t ext_count = glfw_ext_count + 1;
    const char** extensions =
        (const char**)malloc(ext_count * sizeof(const char*));
    memcpy(extensions, glfw_extensions, glfw_ext_count * sizeof(const char*));
    extensions[glfw_ext_count] = VK_EXT_DEBUG_UTILS_EXTENSION_NAME;
#else
    uint32_t ext_count = glfw_ext_count;
    const char** extensions = glfw_extensions;
#endif

    VkInstanceCreateInfo create_info = {
        .sType = VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO,
        .pApplicationInfo = &app_info,
        .enabledExtensionCount = ext_count,
        .ppEnabledExtensionNames = extensions,
    };

#ifdef DEBUG
    const char* validation_layers[] = {"VK_LAYER_KHRONOS_validation"};
    create_info.enabledLayerCount = 1;
    create_info.ppEnabledLayerNames = validation_layers;
#endif

    VkResult result = vkCreateInstance(&create_info, NULL, &g_device.instance);

#ifdef DEBUG
    free(extensions);
#endif

    assert(result == VK_SUCCESS && "failed to create vulkan instance");
}

static void pick_physical_device(void) {
    uint32_t count = 0;
    vkEnumeratePhysicalDevices(g_device.instance, &count, NULL);
    assert(count > 0 && "no vulkan-capable GPU found");

    VkPhysicalDevice* devices =
        (VkPhysicalDevice*)malloc(count * sizeof(VkPhysicalDevice));

    vkEnumeratePhysicalDevices(g_device.instance, &count, devices);

    const char* required_extensions[] = {
        VK_KHR_DYNAMIC_RENDERING_EXTENSION_NAME,
        VK_EXT_SHADER_OBJECT_EXTENSION_NAME,
        VK_KHR_SWAPCHAIN_EXTENSION_NAME,
    };

    uint32_t required_count =
        sizeof(required_extensions) / sizeof(required_extensions[0]);

    for (uint32_t i = 0; i < count; i++) {
        if (!check_device_extension_support(devices[i], required_extensions,
                                            required_count))
            continue;

        if (!find_queue_families(devices[i], VK_NULL_HANDLE)) continue;

        VkPhysicalDeviceDescriptorIndexingFeatures indexing_features = {
            .sType =
                VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_FEATURES,
            .descriptorBindingPartiallyBound = VK_TRUE,
            .runtimeDescriptorArray = VK_TRUE,
            .shaderSampledImageArrayNonUniformIndexing = VK_TRUE,
            .shaderUniformBufferArrayNonUniformIndexing = VK_TRUE,
            .descriptorBindingStorageBufferUpdateAfterBind = VK_TRUE,
            .descriptorBindingUniformBufferUpdateAfterBind = VK_TRUE,
            .pNext = NULL,
        };

        VkPhysicalDeviceSynchronization2Features sync2_features = {
            .sType =
                VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SYNCHRONIZATION_2_FEATURES,
            .pNext = &indexing_features,
            .synchronization2 = VK_TRUE,
        };

        VkPhysicalDeviceDynamicRenderingFeatures dyn_features = {
            .sType =
                VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DYNAMIC_RENDERING_FEATURES,
            .pNext = &sync2_features,
        };

        VkPhysicalDeviceShaderObjectFeaturesEXT shader_obj_features = {
            .sType =
                VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_OBJECT_FEATURES_EXT,
            .pNext = &dyn_features,
        };

        VkPhysicalDeviceFeatures2 features2 = {
            .sType = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2,
            .pNext = &shader_obj_features,
        };

        vkGetPhysicalDeviceFeatures2(devices[i], &features2);

        if (dyn_features.dynamicRendering && shader_obj_features.shaderObject &&
            sync2_features.synchronization2 &&
            indexing_features.descriptorBindingPartiallyBound &&
            indexing_features.descriptorBindingVariableDescriptorCount &&
            indexing_features.runtimeDescriptorArray &&
            indexing_features.shaderSampledImageArrayNonUniformIndexing &&
            indexing_features.shaderUniformBufferArrayNonUniformIndexing &&
            indexing_features.descriptorBindingStorageBufferUpdateAfterBind &&
            indexing_features.descriptorBindingUniformBufferUpdateAfterBind) {
            g_device.physical_device = devices[i];
            break;
        }
    }

    free(devices);

    assert(g_device.physical_device != VK_NULL_HANDLE &&
           "no suitable GPU found");
}

static void create_logical_device(void) {
    uint32_t unique_families[2] = {g_device.graphics_family,
                                   g_device.present_family};
    uint32_t unique_count = (unique_families[0] == unique_families[1]) ? 1 : 2;

    float priority = 1.0f;
    VkDeviceQueueCreateInfo queue_infos[2];

    for (uint32_t i = 0; i < unique_count; i++) {
        queue_infos[i] = (VkDeviceQueueCreateInfo){
            .sType = VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO,
            .queueFamilyIndex = unique_families[i],
            .queueCount = 1,
            .pQueuePriorities = &priority,
        };
    }

    const char* extensions[] = {
        VK_KHR_DYNAMIC_RENDERING_EXTENSION_NAME,
        VK_EXT_SHADER_OBJECT_EXTENSION_NAME,
        VK_KHR_SWAPCHAIN_EXTENSION_NAME,
    };

    VkPhysicalDeviceDescriptorIndexingFeatures indexing_features = {
        .sType = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_FEATURES,
        .descriptorBindingPartiallyBound = VK_TRUE,
        .runtimeDescriptorArray = VK_TRUE,
        .shaderSampledImageArrayNonUniformIndexing = VK_TRUE,
        .shaderUniformBufferArrayNonUniformIndexing = VK_TRUE,
        .descriptorBindingStorageBufferUpdateAfterBind = VK_TRUE,
        .descriptorBindingUniformBufferUpdateAfterBind = VK_TRUE,
        .pNext = NULL,
    };

    VkPhysicalDeviceSynchronization2Features sync2_features = {
        .sType = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SYNCHRONIZATION_2_FEATURES,
        .pNext = &indexing_features,
        .synchronization2 = VK_TRUE,
    };

    VkPhysicalDeviceDynamicRenderingFeatures dyn_features = {
        .sType = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DYNAMIC_RENDERING_FEATURES,
        .pNext = &sync2_features,
        .dynamicRendering = VK_TRUE,
    };

    VkPhysicalDeviceShaderObjectFeaturesEXT shader_obj_features = {
        .sType = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_OBJECT_FEATURES_EXT,
        .pNext = &dyn_features,
        .shaderObject = VK_TRUE,
    };

    VkDeviceCreateInfo create_info = {
        .sType = VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO,
        .pNext = &shader_obj_features,
        .queueCreateInfoCount = unique_count,
        .pQueueCreateInfos = queue_infos,
        .enabledExtensionCount = sizeof(extensions) / sizeof(extensions[0]),
        .ppEnabledExtensionNames = extensions,
    };

    VkResult result = vkCreateDevice(g_device.physical_device, &create_info,
                                     NULL, &g_device.logical_device);
    assert(result == VK_SUCCESS && "failed to create logical device");

    vkGetDeviceQueue(g_device.logical_device, g_device.graphics_family, 0,
                     &g_device.graphics_queue);
    vkGetDeviceQueue(g_device.logical_device, g_device.present_family, 0,
                     &g_device.present_queue);
}

static void create_allocator(void) {
    VmaAllocatorCreateInfo alloc_info = {
        .physicalDevice = g_device.physical_device,
        .device = g_device.logical_device,
        .instance = g_device.instance,
        .vulkanApiVersion = VK_API_VERSION_1_3,
    };

    VkResult result = vmaCreateAllocator(&alloc_info, &g_device.allocator);
    assert(result == VK_SUCCESS && "failed to create VMA allocator");
}

static void create_bindless_descriptors(void) {
    VkDevice dev = g_device.logical_device;

    VkDescriptorSetLayoutBinding bindings[] = {
        {
            .binding = MATERIAL_BINDING,
            .descriptorType = VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,
            .descriptorCount = MAX_BINDLESS_RESOURCES,
            .stageFlags = VK_SHADER_STAGE_ALL,
        },
        {
            .binding = FRAME_PARAMS_BINDING,
            .descriptorType = VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,
            .descriptorCount = MAX_BINDLESS_RESOURCES,
            .stageFlags = VK_SHADER_STAGE_ALL,
        },
        {
            .binding = TEXTURE_BINDING,
            .descriptorType = VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
            .descriptorCount = MAX_BINDLESS_RESOURCES,
            .stageFlags = VK_SHADER_STAGE_ALL,
        },
    };

    VkDescriptorBindingFlags binding_flags[] = {
        VK_DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT |
            VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT,

        VK_DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT |
            VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT,

        VK_DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT,
    };

    VkDescriptorSetLayoutBindingFlagsCreateInfo flags_info = {
        .sType =
            VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_BINDING_FLAGS_CREATE_INFO,
        .bindingCount = 3,
        .pBindingFlags = binding_flags,
    };

    VkDescriptorSetLayoutCreateInfo layout_info = {
        .sType = VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO,
        .pNext = &flags_info,
        .flags = VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT,
        .bindingCount = 3,
        .pBindings = bindings,
    };

    VkResult r = vkCreateDescriptorSetLayout(dev, &layout_info, NULL,
                                             &g_device.descriptor_layout);
    assert(r == VK_SUCCESS);

    VkDescriptorPoolSize pool_sizes[] = {
        {VK_DESCRIPTOR_TYPE_STORAGE_BUFFER, MAX_BINDLESS_RESOURCES},
        {VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER, MAX_BINDLESS_RESOURCES * 2},
        {VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER, MAX_BINDLESS_RESOURCES},
    };

    VkDescriptorPoolCreateInfo pool_info = {
        .sType = VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO,
        .flags = VK_DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT,
        .maxSets = 1,
        .poolSizeCount = 3,
        .pPoolSizes = pool_sizes,
    };

    r = vkCreateDescriptorPool(dev, &pool_info, NULL,
                               &g_device.descriptor_pool);
    assert(r == VK_SUCCESS);

    VkDescriptorSetAllocateInfo set_alloc = {
        .sType = VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO,
        .descriptorPool = g_device.descriptor_pool,
        .descriptorSetCount = 1,
        .pSetLayouts = &g_device.descriptor_layout,
    };

    r = vkAllocateDescriptorSets(dev, &set_alloc, &g_device.descriptor_set);

    assert(r == VK_SUCCESS);

    VkPushConstantRange push_range = {
        .stageFlags = VK_SHADER_STAGE_ALL,
        .offset = 0,
        .size = sizeof(PushConstants),
    };

    VkPipelineLayoutCreateInfo pipeline_layout_info = {
        .sType = VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO,
        .setLayoutCount = 1,
        .pSetLayouts = &g_device.descriptor_layout,
        .pushConstantRangeCount = 1,
        .pPushConstantRanges = &push_range,
    };

    r = vkCreatePipelineLayout(dev, &pipeline_layout_info, NULL,
                               &g_device.pipeline_layout);

    assert(r == VK_SUCCESS);
}

static void create_upload_cmd_pool(void) {
    VkCommandPoolCreateInfo pool_info = {
        .sType = VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO,
        .flags = VK_COMMAND_POOL_CREATE_TRANSIENT_BIT |
                 VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT,
        .queueFamilyIndex = g_device.graphics_family,
    };

    vkCreateCommandPool(g_device.logical_device, &pool_info, NULL,
                        &g_device.upload_cmd_pool);
}

static VkSampleCountFlagBits get_max_usable_sample_count(void) {
    VkPhysicalDeviceProperties properties;
    vkGetPhysicalDeviceProperties(g_device.physical_device, &properties);

    VkSampleCountFlags counts = properties.limits.framebufferColorSampleCounts &
                                properties.limits.framebufferDepthSampleCounts;

    if (counts & VK_SAMPLE_COUNT_64_BIT) return VK_SAMPLE_COUNT_64_BIT;
    if (counts & VK_SAMPLE_COUNT_32_BIT) return VK_SAMPLE_COUNT_32_BIT;
    if (counts & VK_SAMPLE_COUNT_16_BIT) return VK_SAMPLE_COUNT_16_BIT;
    if (counts & VK_SAMPLE_COUNT_8_BIT) return VK_SAMPLE_COUNT_8_BIT;
    if (counts & VK_SAMPLE_COUNT_4_BIT) return VK_SAMPLE_COUNT_4_BIT;
    if (counts & VK_SAMPLE_COUNT_2_BIT) return VK_SAMPLE_COUNT_2_BIT;

    return VK_SAMPLE_COUNT_1_BIT;
}

void init_device() {
    create_instance();
#ifdef DEBUG
    create_debug_messenger();
#endif
    pick_physical_device();
    create_logical_device();
    create_allocator();
    create_bindless_descriptors();
    create_upload_cmd_pool();

    g_device.max_sample_count = get_max_usable_sample_count();
}

void destroy_device() {
    VkDevice dev = g_device.logical_device;

    vkDeviceWaitIdle(dev);

    if (dev != VK_NULL_HANDLE) {
        vkDestroyPipelineLayout(dev, g_device.pipeline_layout, NULL);
        vkDestroyDescriptorPool(dev, g_device.descriptor_pool, NULL);
        vkDestroyDescriptorSetLayout(dev, g_device.descriptor_layout, NULL);
        vkDestroyCommandPool(dev, g_device.upload_cmd_pool, NULL);
    }

    if (g_device.allocator != VK_NULL_HANDLE) {
        vmaDestroyAllocator(g_device.allocator);
    }
    if (dev != VK_NULL_HANDLE) {
        vkDestroyDevice(dev, NULL);
    }
#ifdef DEBUG
    destroy_debug_messenger();
#endif
    if (g_device.instance != VK_NULL_HANDLE) {
        vkDestroyInstance(g_device.instance, NULL);
    }
    memset(&g_device, 0, sizeof(g_device));
}

VkCommandBuffer begin_single_time_commands(void) {
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

int end_single_time_commands(VkCommandBuffer cmd) {
    int ok = 0;
    enif_mutex_lock(g_vk_mutex);

    VkResult r = vkEndCommandBuffer(cmd);
    if (r != VK_SUCCESS) goto out;

    VkCommandBufferSubmitInfo cmd_info = {
        .sType = VK_STRUCTURE_TYPE_COMMAND_BUFFER_SUBMIT_INFO,
        .commandBuffer = cmd,
    };

    VkSubmitInfo2 submit = {
        .sType = VK_STRUCTURE_TYPE_SUBMIT_INFO_2,
        .pCommandBufferInfos = &cmd_info,
        .commandBufferInfoCount = 1,
    };

    r = vkQueueSubmit2(g_device.graphics_queue, 1, &submit, VK_NULL_HANDLE);
    if (r != VK_SUCCESS) goto out;

    r = vkDeviceWaitIdle(g_device.logical_device);
    if (r != VK_SUCCESS) goto out;

    vkFreeCommandBuffers(g_device.logical_device, g_device.upload_cmd_pool, 1,
                         &cmd);
    ok = 1;

out:
    enif_mutex_unlock(g_vk_mutex);
    return ok;
}
