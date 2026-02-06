#include "window.h"

#include <GLFW/glfw3.h>

#include "device.h"

static VkSurfaceFormatKHR choose_surface_format(VkSurfaceKHR surface) {
    uint32_t count = 0;
    vkGetPhysicalDeviceSurfaceFormatsKHR(g_device.physical_device, surface,
                                         &count, NULL);

    VkSurfaceFormatKHR* formats =
        (VkSurfaceFormatKHR*)malloc(count * sizeof(VkSurfaceFormatKHR));
    vkGetPhysicalDeviceSurfaceFormatsKHR(g_device.physical_device, surface,
                                         &count, formats);

    VkSurfaceFormatKHR chosen = formats[0];

    for (uint32_t i = 0; i < count; i++) {
        if (formats[i].format == VK_FORMAT_B8G8R8A8_SRGB &&
            formats[i].colorSpace == VK_COLOR_SPACE_SRGB_NONLINEAR_KHR) {
            chosen = formats[i];
            break;
        }
    }

    free(formats);
    return chosen;
}

static VkPresentModeKHR choose_present_mode(VkSurfaceKHR surface) {
    uint32_t count = 0;
    vkGetPhysicalDeviceSurfacePresentModesKHR(g_device.physical_device, surface,
                                              &count, NULL);

    VkPresentModeKHR* modes =
        (VkPresentModeKHR*)malloc(count * sizeof(VkPresentModeKHR));
    vkGetPhysicalDeviceSurfacePresentModesKHR(g_device.physical_device, surface,
                                              &count, modes);

    VkPresentModeKHR chosen = VK_PRESENT_MODE_FIFO_KHR;  // guaranteed available

    for (uint32_t i = 0; i < count; i++) {
        if (modes[i] == VK_PRESENT_MODE_MAILBOX_KHR) {
            chosen = VK_PRESENT_MODE_MAILBOX_KHR;
            break;
        }
    }

    free(modes);
    return chosen;
}

static VkExtent2D choose_extent(GLFWwindow* handle,
                                VkSurfaceCapabilitiesKHR* caps) {
    if (caps->currentExtent.width != UINT32_MAX) {
        return caps->currentExtent;
    }

    int width, height;
    glfwGetFramebufferSize(handle, &width, &height);

    VkExtent2D extent = {
        .width = (uint32_t)width,
        .height = (uint32_t)height,
    };

    if (extent.width < caps->minImageExtent.width)
        extent.width = caps->minImageExtent.width;
    if (extent.width > caps->maxImageExtent.width)
        extent.width = caps->maxImageExtent.width;
    if (extent.height < caps->minImageExtent.height)
        extent.height = caps->minImageExtent.height;
    if (extent.height > caps->maxImageExtent.height)
        extent.height = caps->maxImageExtent.height;

    return extent;
}

static void create_swapchain(struct Window* window) {
    VkSurfaceCapabilitiesKHR caps;
    vkGetPhysicalDeviceSurfaceCapabilitiesKHR(g_device.physical_device,
                                              window->surface, &caps);

    VkSurfaceFormatKHR surface_format = choose_surface_format(window->surface);
    VkPresentModeKHR present_mode = choose_present_mode(window->surface);
    VkExtent2D extent = choose_extent(window->handle, &caps);

    uint32_t image_count = caps.minImageCount + 1;
    if (caps.maxImageCount > 0 && image_count > caps.maxImageCount) {
        image_count = caps.maxImageCount;
    }

    VkSwapchainCreateInfoKHR create_info = {
        .sType = VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR,
        .surface = window->surface,
        .minImageCount = image_count,
        .imageFormat = surface_format.format,
        .imageColorSpace = surface_format.colorSpace,
        .imageExtent = extent,
        .imageArrayLayers = 1,
        .imageUsage = VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT,
        .preTransform = caps.currentTransform,
        .compositeAlpha = VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR,
        .presentMode = present_mode,
        .clipped = VK_TRUE,
        .oldSwapchain = VK_NULL_HANDLE,
    };

    uint32_t queue_families[] = {g_device.graphics_family,
                                 g_device.present_family};

    if (g_device.graphics_family != g_device.present_family) {
        create_info.imageSharingMode = VK_SHARING_MODE_CONCURRENT;
        create_info.queueFamilyIndexCount = 2;
        create_info.pQueueFamilyIndices = queue_families;
    } else {
        create_info.imageSharingMode = VK_SHARING_MODE_EXCLUSIVE;
    }

    VkResult result = vkCreateSwapchainKHR(
        g_device.logical_device, &create_info, NULL, &window->swapchain);
    assert(result == VK_SUCCESS && "failed to create swapchain");

    window->swapchain_format = surface_format.format;
    window->swapchain_extent = extent;
}

static void get_swapchain_images(struct Window* window) {
    vkGetSwapchainImagesKHR(g_device.logical_device, window->swapchain,
                            &window->image_count, NULL);

    window->images = (VkImage*)malloc(window->image_count * sizeof(VkImage));
    vkGetSwapchainImagesKHR(g_device.logical_device, window->swapchain,
                            &window->image_count, window->images);
}

static void create_image_views(struct Window* window) {
    window->image_views =
        (VkImageView*)malloc(window->image_count * sizeof(VkImageView));

    for (uint32_t i = 0; i < window->image_count; i++) {
        VkImageViewCreateInfo create_info = {
            .sType = VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO,
            .image = window->images[i],
            .viewType = VK_IMAGE_VIEW_TYPE_2D,
            .format = window->swapchain_format,
            .components =
                {
                    .r = VK_COMPONENT_SWIZZLE_IDENTITY,
                    .g = VK_COMPONENT_SWIZZLE_IDENTITY,
                    .b = VK_COMPONENT_SWIZZLE_IDENTITY,
                    .a = VK_COMPONENT_SWIZZLE_IDENTITY,
                },
            .subresourceRange =
                {
                    .aspectMask = VK_IMAGE_ASPECT_COLOR_BIT,
                    .baseMipLevel = 0,
                    .levelCount = 1,
                    .baseArrayLayer = 0,
                    .layerCount = 1,
                },
        };

        VkResult result =
            vkCreateImageView(g_device.logical_device, &create_info, NULL,
                              &window->image_views[i]);
        assert(result == VK_SUCCESS && "failed to create image view");
    }
}

struct Window* create_window(const char* title, size_t width, size_t height) {
    struct Window* window = (struct Window*)calloc(1, sizeof(struct Window));

    glfwWindowHint(GLFW_CLIENT_API, GLFW_NO_API);
    glfwWindowHint(GLFW_RESIZABLE, GLFW_FALSE);
    window->handle =
        glfwCreateWindow((int)width, (int)height, title, NULL, NULL);
    assert(window->handle != NULL && "failed to create GLFW window");

    VkResult result = glfwCreateWindowSurface(g_device.instance, window->handle,
                                              NULL, &window->surface);

    assert(result == VK_SUCCESS && "failed to create window surface");

    create_swapchain(window);
    get_swapchain_images(window);
    create_image_views(window);

    return window;
}

int window_should_close(struct Window* window) {
    glfwPollEvents();
    return glfwWindowShouldClose(window->handle);
}

void destroy_window(struct Window* window) {
    if (!window) return;

    for (uint32_t i = 0; i < window->image_count; i++) {
        vkDestroyImageView(g_device.logical_device, window->image_views[i],
                           NULL);
    }

    free(window->image_views);
    free(window->images);

    vkDestroySwapchainKHR(g_device.logical_device, window->swapchain, NULL);
    vkDestroySurfaceKHR(g_device.instance, window->surface, NULL);
    glfwDestroyWindow(window->handle);

    free(window);
}
