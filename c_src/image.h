#pragma once

#include <vulkan/vulkan.h>
#include "erl_nif.h"

#include "vk_mem_alloc.h"

#define FLUO_COLOR_FORMAT VK_FORMAT_R8G8B8A8_UNORM

#define FLUO_DEPTH_FORMAT VK_FORMAT_D32_SFLOAT

#define FLUO_COLOR_IMAGE_USAGE \
    VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT | VK_IMAGE_USAGE_TRANSFER_SRC_BIT | VK_IMAGE_USAGE_TRANSFER_DST_BIT

#define FLUO_DEPTH_IMAGE_USAGE \
    VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT | VK_IMAGE_USAGE_TRANSFER_SRC_BIT | VK_IMAGE_USAGE_TRANSFER_DST_BIT

typedef struct {
    VkImage image;
    VkImageView view;
    VkImageAspectFlags aspect;
    VkFormat format;
    VkImageLayout optimal_layout;
    VkImageLayout current_layout;
    VmaAllocation alloc;
    VkExtent2D extent;
} image_res_t;

int create_color_image(image_res_t* out,
                       VkImageLayout optimal_layout,
                       uint32_t width,
                       uint32_t height,
                       VkImageUsageFlags usage,
                       VkMemoryPropertyFlags memory_properties);

int create_depth_image(image_res_t* out,
                       VkImageLayout optimal_layout,
                       uint32_t width,
                       uint32_t height,
                       VkImageUsageFlags usage,
                       VkMemoryPropertyFlags memory_properties);

void destroy_gpu_image(image_res_t* img);

void transition_image_layout(image_res_t* img, VkImageLayout newLayout, VkCommandBuffer cmd);

void transition_iamge_to_optimal_layout(image_res_t* img, VkCommandBuffer cmd);

void blit_image(image_res_t src, image_res_t dst, VkCommandBuffer cmd);

int nif_init_image_res(ErlNifEnv* env);

image_res_t* alloc_image_res(ErlNifEnv* env);

int create_image(image_res_t* out,
                 uint32_t width,
                 uint32_t height,
                 VkImageLayout optimal_layout,
                 VkFormat format,
                 VkImageUsageFlags usage,
                 VkImageAspectFlags aspect,
                 VkMemoryPropertyFlags memory_properties);

image_res_t* get_image_from_term(ErlNifEnv* env, ERL_NIF_TERM term);

ERL_NIF_TERM nif_create_depth_image(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

ERL_NIF_TERM nif_create_color_image(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

ERL_NIF_TERM nif_read_image(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

ERL_NIF_TERM nif_save_color_image_to_png(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

image_res_t* get_image_from_option(ErlNifEnv* env, ERL_NIF_TERM term);

extern VkCommandPool g_blit_cmd_pool;

void destroy_blit_command_pool(void);
