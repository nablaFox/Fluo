#pragma once

#include <erl_nif.h>
#include <vulkan/vulkan.h>
#include <GLFW/glfw3.h>
#include "image.h"

typedef struct {
    GLFWwindow* handle;
    VkSurfaceKHR surface;

    VkSwapchainKHR swapchain;
    VkFormat swapchain_format;
    VkExtent2D swapchain_extent;

    image_res_t* swapchain_images;
    uint32_t swapchain_image_count;

    VkSemaphore* image_available_sem;
    VkSemaphore* finished_blitting_sem;
    VkCommandPool blit_cmd_pool;
    VkCommandBuffer* blit_cmds;
    VkFence* blit_fences;
} window_res_t;

int nif_init_window_res(ErlNifEnv* env);

ERL_NIF_TERM nif_create_window(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

ERL_NIF_TERM nif_window_should_close(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

window_res_t* get_window_from_term(ErlNifEnv* env, ERL_NIF_TERM term);

uint32_t get_curr_swapchain_idx(const window_res_t* w, VkSemaphore signal_sem);
