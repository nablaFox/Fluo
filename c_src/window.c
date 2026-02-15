#include "window.h"

#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <vulkan/vulkan_core.h>

#include "command.h"
#include "device.h"
#include "utils.h"

static ErlNifResourceType* WINDOW_RES_TYPE = NULL;

static void cursor_pos_cb(GLFWwindow* win, double x, double y) {
    window_res_t* w = (window_res_t*)glfwGetWindowUserPointer(win);
    if (!w) return;

    w->curr_cb_x = x;
    w->curr_cb_y = y;

    if (!w->has_last_cb) {
        w->has_last_cb = 1;
        w->last_cb_x = x;
        w->last_cb_y = y;
        return;
    }

    w->accum_dx += (x - w->last_cb_x);
    w->accum_dy += (y - w->last_cb_y);

    w->last_cb_x = x;
    w->last_cb_y = y;
}

static inline void reset_mouse_tracking(window_res_t* w) {
    if (!w) return;

    w->accum_dx = 0.0;
    w->accum_dy = 0.0;

    w->has_last_cb = 0;
    w->last_cb_x = 0.0;
    w->last_cb_y = 0.0;
}

static VkSurfaceFormatKHR choose_surface_format(VkSurfaceKHR surface) {
    uint32_t count = 0;
    vkGetPhysicalDeviceSurfaceFormatsKHR(g_device.physical_device, surface,
                                         &count, NULL);
    assert(count > 0);

    VkSurfaceFormatKHR* formats =
        (VkSurfaceFormatKHR*)malloc(count * sizeof(VkSurfaceFormatKHR));
    assert(formats);

    vkGetPhysicalDeviceSurfaceFormatsKHR(g_device.physical_device, surface,
                                         &count, formats);

    VkSurfaceFormatKHR chosen = formats[0];
    for (uint32_t i = 0; i < count; i++) {
        if (formats[i].format == FLUO_SURFACE_FORMAT &&
            formats[i].colorSpace == FLUO_SURFACE_COLOR_SPACE) {
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
    assert(count > 0);

    VkPresentModeKHR* modes =
        (VkPresentModeKHR*)malloc(count * sizeof(VkPresentModeKHR));
    assert(modes);

    vkGetPhysicalDeviceSurfacePresentModesKHR(g_device.physical_device, surface,
                                              &count, modes);

    VkPresentModeKHR chosen = VK_PRESENT_MODE_FIFO_KHR;

    for (uint32_t i = 0; i < count; i++) {
        if (modes[i] == FLUO_PRESENT_MODE) {
            chosen = FLUO_PRESENT_MODE;
            break;
        }
    }

    free(modes);
    return chosen;
}

static VkExtent2D choose_extent(GLFWwindow* handle,
                                const VkSurfaceCapabilitiesKHR* caps) {
    if (caps->currentExtent.width != UINT32_MAX) {
        return caps->currentExtent;
    }

    int width = 0, height = 0;
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

static void destroy_swapchain_image_views(window_res_t* w) {
    if (!w || !w->swapchain_images) return;

    for (uint32_t i = 0; i < w->swapchain_image_count; i++) {
        if (w->swapchain_images[i].view) {
            vkDestroyImageView(g_device.logical_device,
                               w->swapchain_images[i].view, NULL);
            w->swapchain_images[i].view = VK_NULL_HANDLE;
        }

        w->swapchain_images[i].image = VK_NULL_HANDLE;
        w->swapchain_images[i].alloc = NULL;
        w->swapchain_images[i].current_layout = VK_IMAGE_LAYOUT_UNDEFINED;
        w->swapchain_images[i].optimal_layout = VK_IMAGE_LAYOUT_UNDEFINED;
    }

    free(w->swapchain_images);
    w->swapchain_images = NULL;
    w->swapchain_image_count = 0;
}

static void create_swapchain(window_res_t* w) {
    VkSurfaceCapabilitiesKHR caps;
    vkGetPhysicalDeviceSurfaceCapabilitiesKHR(g_device.physical_device,
                                              w->surface, &caps);

    VkSurfaceFormatKHR surface_format = choose_surface_format(w->surface);
    VkPresentModeKHR present_mode = choose_present_mode(w->surface);
    VkExtent2D extent = choose_extent(w->handle, &caps);

    uint32_t image_count = caps.minImageCount + 1;
    if (caps.maxImageCount > 0 && image_count > caps.maxImageCount) {
        image_count = caps.maxImageCount;
    }

    VkSwapchainCreateInfoKHR ci = {
        .sType = VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR,
        .surface = w->surface,
        .minImageCount = image_count,
        .imageFormat = surface_format.format,
        .imageColorSpace = surface_format.colorSpace,
        .imageExtent = extent,
        .imageArrayLayers = 1,
        .imageUsage = VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT |
                      VK_IMAGE_USAGE_TRANSFER_DST_BIT,
        .preTransform = caps.currentTransform,
        .compositeAlpha = VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR,
        .presentMode = present_mode,
        .clipped = VK_TRUE,
        .oldSwapchain = VK_NULL_HANDLE,
    };

    uint32_t queue_families[] = {g_device.graphics_family,
                                 g_device.present_family};
    if (g_device.graphics_family != g_device.present_family) {
        ci.imageSharingMode = VK_SHARING_MODE_CONCURRENT;
        ci.queueFamilyIndexCount = 2;
        ci.pQueueFamilyIndices = queue_families;
    } else {
        ci.imageSharingMode = VK_SHARING_MODE_EXCLUSIVE;
    }

    assert(vkCreateSwapchainKHR(g_device.logical_device, &ci, NULL,
                                &w->swapchain) == VK_SUCCESS);

    w->swapchain_format = surface_format.format;
    w->swapchain_extent = extent;
}

static void get_swapchain_images_and_create_views(window_res_t* w) {
    uint32_t count = 0;
    vkGetSwapchainImagesKHR(g_device.logical_device, w->swapchain, &count,
                            NULL);
    assert(count > 0);

    VkImage* images = (VkImage*)malloc(sizeof(VkImage) * count);
    assert(images);

    vkGetSwapchainImagesKHR(g_device.logical_device, w->swapchain, &count,
                            images);

    w->swapchain_images = (image_res_t*)malloc(sizeof(image_res_t) * count);
    assert(w->swapchain_images);
    memset(w->swapchain_images, 0, sizeof(image_res_t) * count);

    w->swapchain_image_count = count;

    for (uint32_t i = 0; i < count; i++) {
        w->swapchain_images[i].image = images[i];
        w->swapchain_images[i].alloc = NULL;
        w->swapchain_images[i].current_layout = VK_IMAGE_LAYOUT_UNDEFINED;
        w->swapchain_images[i].aspect = VK_IMAGE_ASPECT_COLOR_BIT;
        w->swapchain_images[i].extent = w->swapchain_extent;
        w->swapchain_images[i].optimal_layout = VK_IMAGE_LAYOUT_PRESENT_SRC_KHR;

        VkImageViewCreateInfo ci = {
            .sType = VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO,
            .image = images[i],
            .viewType = VK_IMAGE_VIEW_TYPE_2D,
            .format = w->swapchain_format,
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

        assert(vkCreateImageView(g_device.logical_device, &ci, NULL,
                                 &w->swapchain_images[i].view) == VK_SUCCESS);
    }

    free(images);
}

static void window_res_dtor(ErlNifEnv* env, void* obj) {
    (void)env;

    if (g_device.logical_device) vkDeviceWaitIdle(g_device.logical_device);

    window_res_t* w = (window_res_t*)obj;

    if (!w) return;

    VkDevice dev = g_device.logical_device;

    vkDeviceWaitIdle(dev);

    if (w->blit_finished_fence) {
        vkDestroyFence(dev, w->blit_finished_fence, NULL);
        w->blit_finished_fence = VK_NULL_HANDLE;
    }

    if (w->image_available_sem) {
        vkDestroySemaphore(dev, w->image_available_sem, NULL);
        w->image_available_sem = VK_NULL_HANDLE;
    }

    if (w->present_sem) {
        for (uint32_t i = 0; i < w->swapchain_image_count; i++) {
            if (w->present_sem[i])
                vkDestroySemaphore(dev, w->present_sem[i], NULL);
        }

        free(w->present_sem);
        w->present_sem = NULL;
    }

    if (g_blit_cmd_pool != VK_NULL_HANDLE && w->blit_cmd) {
        vkFreeCommandBuffers(dev, g_blit_cmd_pool, 1, &w->blit_cmd);
        w->blit_cmd = VK_NULL_HANDLE;
    }

    destroy_swapchain_image_views(w);

    if (w->swapchain) {
        vkDestroySwapchainKHR(g_device.logical_device, w->swapchain, NULL);
        w->swapchain = VK_NULL_HANDLE;
    }

    if (w->surface) {
        vkDestroySurfaceKHR(g_device.instance, w->surface, NULL);
        w->surface = VK_NULL_HANDLE;
    }

    if (w->handle) {
        glfwSetCursorPosCallback(w->handle, NULL);
        glfwSetWindowUserPointer(w->handle, NULL);
        glfwDestroyWindow(w->handle);
        w->handle = NULL;
    }
}

static uint32_t get_curr_swapchain_idx_fence(const window_res_t* w) {
    if (!w) return UINT32_MAX;
    if (!w->swapchain) return UINT32_MAX;
    if (w->swapchain_image_count == 0 || !w->swapchain_images)
        return UINT32_MAX;

    VkDevice dev = g_device.logical_device;
    if (!dev) return UINT32_MAX;

    uint32_t img_idx = 0;

    VkResult acq =
        vkAcquireNextImageKHR(dev, w->swapchain, UINT64_MAX,
                              w->image_available_sem, VK_NULL_HANDLE, &img_idx);

    if (acq == VK_ERROR_OUT_OF_DATE_KHR) return UINT32_MAX;
    if (acq != VK_SUCCESS && acq != VK_SUBOPTIMAL_KHR) return UINT32_MAX;
    if (img_idx >= w->swapchain_image_count) return UINT32_MAX;

    return img_idx;
}

int nif_init_window_res(ErlNifEnv* env) {
    WINDOW_RES_TYPE =
        enif_open_resource_type(env, "fluo_nif", "window_res", window_res_dtor,
                                ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL);

    if (!WINDOW_RES_TYPE) return -1;

    return 0;
}

window_res_t* get_window_from_term(ErlNifEnv* env, ERL_NIF_TERM term) {
    window_res_t* res = NULL;

    if (!enif_get_resource(env, term, WINDOW_RES_TYPE, (void**)&res))
        return NULL;

    if (!res || !res->handle) return NULL;

    return res;
}

ERL_NIF_TERM nif_create_window(ErlNifEnv* env, int argc,
                               const ERL_NIF_TERM argv[]) {
    if (argc != 3) return enif_make_badarg(env);

    int width = 0, height = 0;
    if (!enif_get_int(env, argv[1], &width)) return enif_make_badarg(env);
    if (!enif_get_int(env, argv[2], &height)) return enif_make_badarg(env);
    if (width <= 0 || height <= 0) return enif_make_badarg(env);

    char* title = NULL;
    if (!get_c_string_from_gleam_string(env, argv[0], &title))
        return enif_make_badarg(env);

    window_res_t* res = (window_res_t*)enif_alloc_resource(
        WINDOW_RES_TYPE, sizeof(window_res_t));

    if (!res) {
        free(title);
        return enif_make_badarg(env);
    }

    memset(res, 0, sizeof(*res));

    glfwWindowHint(GLFW_CLIENT_API, GLFW_NO_API);
    glfwWindowHint(GLFW_RESIZABLE, GLFW_FALSE);
    glfwWindowHint(GLFW_DECORATED, GLFW_FALSE);

    res->handle = glfwCreateWindow(width, height, title, NULL, NULL);

    free(title);
    title = NULL;

    if (!res->handle) {
        enif_release_resource(res);
        return enif_make_badarg(env);
    }

    reset_mouse_tracking(res);
    glfwSetWindowUserPointer(res->handle, res);
    glfwSetCursorPosCallback(res->handle, cursor_pos_cb);

    glfwSetInputMode(res->handle, GLFW_CURSOR, GLFW_CURSOR_DISABLED);
    glfwSetInputMode(res->handle, GLFW_RAW_MOUSE_MOTION, GLFW_TRUE);

    res->last_time = 0.0;
    res->has_last_time = 0;

    if (glfwCreateWindowSurface(g_device.instance, res->handle, NULL,
                                &res->surface) != VK_SUCCESS) {
        enif_release_resource(res);
        return enif_make_badarg(env);
    }

    create_swapchain(res);
    get_swapchain_images_and_create_views(res);

    VkDevice dev = g_device.logical_device;

    VkCommandBufferAllocateInfo alloc_ci = {
        .sType = VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO,
        .commandPool = g_blit_cmd_pool,
        .level = VK_COMMAND_BUFFER_LEVEL_PRIMARY,
        .commandBufferCount = 1,
    };

    if (vkAllocateCommandBuffers(dev, &alloc_ci, &res->blit_cmd) !=
        VK_SUCCESS) {
        enif_release_resource(res);
        return enif_make_badarg(env);
    }

    VkFenceCreateInfo fence_ci = {
        .sType = VK_STRUCTURE_TYPE_FENCE_CREATE_INFO,
        .flags = VK_FENCE_CREATE_SIGNALED_BIT,
    };

    if (vkCreateFence(dev, &fence_ci, NULL, &res->blit_finished_fence) !=
        VK_SUCCESS) {
        enif_release_resource(res);
        return enif_make_badarg(env);
    }

    VkSemaphoreCreateInfo sem_ci = {
        .sType = VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO,
    };

    res->present_sem = malloc(sizeof(VkSemaphore) * res->swapchain_image_count);

    assert(res->present_sem);

    for (uint32_t i = 0; i < res->swapchain_image_count; i++) {
        THROW_VK_ERROR(
            env, vkCreateSemaphore(dev, &sem_ci, NULL, &res->present_sem[i]));
    }

    if (vkCreateSemaphore(dev, &sem_ci, NULL, &res->image_available_sem) !=
        VK_SUCCESS) {
        enif_release_resource(res);
        return enif_make_badarg(env);
    }

    ERL_NIF_TERM handle_term = enif_make_resource(env, res);

    enif_release_resource(res);

    return handle_term;
}

ERL_NIF_TERM nif_window_should_close(ErlNifEnv* env, int argc,
                                     const ERL_NIF_TERM argv[]) {
    window_res_t* res = get_window_from_term(env, argv[0]);

    if (!res) return enif_make_badarg(env);

    glfwPollEvents();

    const int should_close = glfwWindowShouldClose(res->handle);

    return should_close ? enif_make_atom(env, "true")
                        : enif_make_atom(env, "false");
}

ERL_NIF_TERM nif_window_delta_time(ErlNifEnv* env, int argc,
                                   const ERL_NIF_TERM argv[]) {
    window_res_t* w = get_window_from_term(env, argv[0]);

    if (!w || !w->handle) return enif_make_badarg(env);

    double now = glfwGetTime();

    if (!w->has_last_time) {
        w->has_last_time = 1;
        w->last_time = now;
        return enif_make_double(env, 0.0);
    }

    double dt = now - w->last_time;
    w->last_time = now;

    if (dt < 0.0) dt = 0.0;
    if (dt > 0.1) dt = 0.1;

    return enif_make_double(env, dt);
}

ERL_NIF_TERM nif_swap_buffers(ErlNifEnv* env, int argc,
                              const ERL_NIF_TERM argv[]) {
    window_res_t* window = get_window_from_term(env, argv[0]);
    if (!window) return enif_make_badarg(env);

    image_res_t* color_image = get_image_from_term(env, argv[1]);
    if (!color_image) return enif_make_badarg(env);

    const command_res_t* cmd_res = get_command_from_term(env, argv[2]);

    if (!cmd_res) return enif_make_badarg(env);

    enif_mutex_lock(g_vk_mutex);

    VkSemaphore cmd_finished_sem =
        cmd_res->finished_sem[cmd_res->last_submitted_frame];

    VkDevice dev = g_device.logical_device;

    VkCommandBuffer blit_cmd = window->blit_cmd;

    VkFence fence = window->blit_finished_fence;

    THROW_VK_ERROR(env, vkWaitForFences(dev, 1, &fence, VK_TRUE, UINT64_MAX));
    THROW_VK_ERROR(env, vkResetFences(dev, 1, &fence));

    uint32_t img_idx = get_curr_swapchain_idx_fence(window);

    if (img_idx == UINT32_MAX) {
        return enif_make_atom(env, "out_of_date");
    }

    THROW_VK_ERROR(env, vkResetCommandBuffer(blit_cmd, 0));

    VkCommandBufferBeginInfo begin = {
        .sType = VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO,
        .flags = VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT,
    };

    THROW_VK_ERROR(env, vkBeginCommandBuffer(blit_cmd, &begin));

    image_res_t* swapchain_image = &window->swapchain_images[img_idx];

    transition_image_layout(color_image, VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
                            blit_cmd);

    transition_image_layout(swapchain_image,
                            VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL, blit_cmd);

    VkImageResolve2 region = {
        .sType = VK_STRUCTURE_TYPE_IMAGE_RESOLVE_2,
        .srcSubresource =
            {
                .aspectMask = VK_IMAGE_ASPECT_COLOR_BIT,
                .mipLevel = 0,
                .baseArrayLayer = 0,
                .layerCount = 1,
            },
        .srcOffset = {0, 0, 0},
        .dstSubresource =
            {
                .aspectMask = VK_IMAGE_ASPECT_COLOR_BIT,
                .mipLevel = 0,
                .baseArrayLayer = 0,
                .layerCount = 1,
            },
        .dstOffset = {0, 0, 0},
        .extent =
            {
                .width = swapchain_image->extent.width,
                .height = swapchain_image->extent.height,
                .depth = 1,
            },
    };

    VkResolveImageInfo2 resolve_info = {
        .sType = VK_STRUCTURE_TYPE_RESOLVE_IMAGE_INFO_2,
        .srcImage = color_image->image,
        .srcImageLayout = VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
        .dstImage = swapchain_image->image,
        .dstImageLayout = VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
        .regionCount = 1,
        .pRegions = &region,
    };

    vkCmdResolveImage2(blit_cmd, &resolve_info);

    transition_iamge_to_optimal_layout(swapchain_image, blit_cmd);
    transition_iamge_to_optimal_layout(color_image, blit_cmd);

    THROW_VK_ERROR(env, vkEndCommandBuffer(blit_cmd));

    VkSemaphoreSubmitInfo wait_for_blit[] = {
        {
            .sType = VK_STRUCTURE_TYPE_SEMAPHORE_SUBMIT_INFO,
            .stageMask = VK_PIPELINE_STAGE_2_TRANSFER_BIT,
            .semaphore = cmd_finished_sem,
            .value = 0,
        },

        {
            .sType = VK_STRUCTURE_TYPE_SEMAPHORE_SUBMIT_INFO,
            .stageMask = VK_PIPELINE_STAGE_2_TRANSFER_BIT,
            .semaphore = window->image_available_sem,
            .value = 0,
        },
    };

    VkSemaphore present_sem = window->present_sem[img_idx];

    VkSemaphoreSubmitInfo signal_finished_blit = {
        .sType = VK_STRUCTURE_TYPE_SEMAPHORE_SUBMIT_INFO,
        .semaphore = present_sem,
        .stageMask = VK_PIPELINE_STAGE_2_TRANSFER_BIT,
        .value = 0,
    };

    VkCommandBufferSubmitInfo cmd_info = {
        .sType = VK_STRUCTURE_TYPE_COMMAND_BUFFER_SUBMIT_INFO,
        .commandBuffer = blit_cmd,
        .deviceMask = 0,
    };

    VkSubmitInfo2 blit_submit = {
        .sType = VK_STRUCTURE_TYPE_SUBMIT_INFO_2,
        .waitSemaphoreInfoCount = 2,
        .pWaitSemaphoreInfos = wait_for_blit,
        .commandBufferInfoCount = 1,
        .pCommandBufferInfos = &cmd_info,
        .signalSemaphoreInfoCount = 1,
        .pSignalSemaphoreInfos = &signal_finished_blit,
    };

    THROW_VK_ERROR(
        env, vkQueueSubmit2(g_device.graphics_queue, 1, &blit_submit, fence));

    VkPresentInfoKHR present_submit = {
        .sType = VK_STRUCTURE_TYPE_PRESENT_INFO_KHR,
        .waitSemaphoreCount = 1,
        .pWaitSemaphores = &present_sem,
        .swapchainCount = 1,
        .pSwapchains = &window->swapchain,
        .pImageIndices = &img_idx,
    };

    VkResult pr = vkQueuePresentKHR(g_device.present_queue, &present_submit);

    if (pr == VK_ERROR_OUT_OF_DATE_KHR)
        return enif_make_atom(env, "out_of_date");

    if (pr != VK_SUCCESS && pr != VK_SUBOPTIMAL_KHR) {
        THROW_VK_ERROR(env, pr);
    }

    enif_mutex_unlock(g_vk_mutex);

    return enif_make_atom(env, "ok");
}
