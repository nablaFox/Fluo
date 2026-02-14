#pragma once

#include <erl_nif.h>
#include <vulkan/vulkan.h>
#include <GLFW/glfw3.h>
#include "image.h"

#define FLUO_PRESENT_MODE VK_PRESENT_MODE_FIFO_RELAXED_KHR

#define FLUO_SURFACE_FORMAT VK_FORMAT_R8G8B8A8_UNORM

typedef struct {
    GLFWwindow* handle;
    VkSurfaceKHR surface;

    VkSwapchainKHR swapchain;
    VkFormat swapchain_format;
    VkExtent2D swapchain_extent;

    image_res_t* swapchain_images;
    uint32_t swapchain_image_count;

    double accum_dx, accum_dy;
    int has_last_cb;
    double last_cb_x, last_cb_y;
    double curr_cb_x;
    double curr_cb_y;

    double last_time;
    int has_last_time;

    VkCommandBuffer blit_cmd;
    VkFence blit_fence;
} window_res_t;

int nif_init_window_res(ErlNifEnv* env);

ERL_NIF_TERM nif_create_window(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

ERL_NIF_TERM nif_window_should_close(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

ERL_NIF_TERM nif_window_keys_down(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

ERL_NIF_TERM nif_window_mouse_pos(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

ERL_NIF_TERM nif_window_mouse_delta(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

ERL_NIF_TERM nif_window_delta_time(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

ERL_NIF_TERM nif_window_capture_mouse(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

ERL_NIF_TERM nif_window_release_mouse(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

ERL_NIF_TERM nif_swap_buffers(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

window_res_t* get_window_from_term(ErlNifEnv* env, ERL_NIF_TERM term);

uint32_t get_curr_swapchain_idx(const window_res_t* w, VkSemaphore signal_sem);
