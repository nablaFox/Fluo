#pragma once

#include <vulkan/vulkan.h>
#include <erl_nif.h>

#include "vk_mem_alloc.h"

typedef struct {
    VkImage image;
    VkImageView view;
    VkImageAspectFlags aspect;
    VkFormat format;
    VkImageLayout current_layout;
    VmaAllocation alloc;
    VkExtent2D extent;
} image_res_t;

// just an image with color format
int create_color_image(image_res_t* out,
                       uint32_t width,
                       uint32_t height,
                       VkImageUsageFlags usage,
                       VkMemoryPropertyFlags memory_properties);

// just an image with depth format
int create_depth_image(image_res_t* out,
                       uint32_t width,
                       uint32_t height,
                       VkImageUsageFlags usage,
                       VkMemoryPropertyFlags memory_properties);

void destroy_gpu_image(image_res_t* img);

void transition_image_layout(image_res_t* img, VkImageLayout newLayout, VkCommandBuffer cmd);

void blit_image(image_res_t src, image_res_t dst, VkCommandBuffer cmd);

int nif_init_image_res(ErlNifEnv* env);

image_res_t* alloc_image_res(ErlNifEnv* env);

int create_image(image_res_t* out,
                 uint32_t width,
                 uint32_t height,
                 VkFormat format,
                 VkImageUsageFlags usage,
                 VkImageAspectFlags aspect,
                 VkMemoryPropertyFlags memory_properties);

image_res_t* get_image_from_term(ErlNifEnv* env, ERL_NIF_TERM term);

ERL_NIF_TERM nif_create_depth_image(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

ERL_NIF_TERM nif_create_color_image(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

ERL_NIF_TERM nif_read_image(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
