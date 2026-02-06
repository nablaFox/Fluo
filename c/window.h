#pragma once

#define GLFW_INCLUDE_VULKAN
#include <GLFW/glfw3.h>

struct Window {
    GLFWwindow* handle;
    VkSurfaceKHR surface;

    VkSwapchainKHR swapchain;
    VkFormat swapchain_format;
    VkExtent2D swapchain_extent;
    uint32_t image_count;
    VkImage* images;
    VkImageView* image_views;
};

struct Window* create_window(const char* title, size_t width, size_t height);

int window_should_close(struct Window* window);

void destroy_window(struct Window* window);
