#pragma once

#include <erl_nif.h>
#include <vulkan/vulkan.h>
#include <GLFW/glfw3.h>
#include <stdint.h>

typedef struct {
    GLFWwindow* handle;
    VkSurfaceKHR surface;

    VkSwapchainKHR swapchain;
    VkFormat swapchain_format;
    VkExtent2D swapchain_extent;

    uint32_t image_count;
    VkImage* images;
    VkImageView* image_views;
} window_res_t;

ERL_NIF_TERM nif_create_window(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

ERL_NIF_TERM nif_window_should_close(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

int nif_init_window_res(ErlNifEnv* env);
