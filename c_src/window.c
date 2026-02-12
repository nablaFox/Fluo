#include "window.h"

#include <vulkan/vulkan_core.h>

#include "device.h"
#include "rendering.h"
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

static int create_sync_structs(window_res_t* w) {
    VkDevice dev = g_device.logical_device;

    w->image_available_sem = malloc(sizeof(VkSemaphore) * FRAMES_IN_FLIGHT);

    w->finished_blitting_sem =
        malloc(sizeof(VkSemaphore) * w->swapchain_image_count);

    w->blit_fences = malloc(sizeof(VkFence) * FRAMES_IN_FLIGHT);

    if (!w->image_available_sem || !w->finished_blitting_sem || !w->blit_fences)
        goto fail_alloc;

    memset(w->image_available_sem, 0, sizeof(VkSemaphore) * FRAMES_IN_FLIGHT);
    memset(w->finished_blitting_sem, 0,
           sizeof(VkSemaphore) * w->swapchain_image_count);
    memset(w->blit_fences, 0, sizeof(VkFence) * FRAMES_IN_FLIGHT);

    VkSemaphoreCreateInfo sem_ci = {
        .sType = VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO};
    VkFenceCreateInfo fence_ci = {
        .sType = VK_STRUCTURE_TYPE_FENCE_CREATE_INFO,
        .flags = VK_FENCE_CREATE_SIGNALED_BIT,
    };

    for (uint32_t i = 0; i < FRAMES_IN_FLIGHT; i++) {
        if (vkCreateSemaphore(dev, &sem_ci, NULL, &w->image_available_sem[i]) !=
            VK_SUCCESS)
            goto fail;

        if (vkCreateFence(dev, &fence_ci, NULL, &w->blit_fences[i]) !=
            VK_SUCCESS)
            goto fail;
    }

    for (uint32_t i = 0; i < w->swapchain_image_count; i++) {
        if (vkCreateSemaphore(dev, &sem_ci, NULL,
                              &w->finished_blitting_sem[i]) != VK_SUCCESS)
            goto fail;
    }

    return 0;

fail:
    for (uint32_t i = 0; i < FRAMES_IN_FLIGHT; i++) {
        if (w->blit_fences && w->blit_fences[i])
            vkDestroyFence(dev, w->blit_fences[i], NULL);
        if (w->image_available_sem && w->image_available_sem[i])
            vkDestroySemaphore(dev, w->image_available_sem[i], NULL);
    }

    for (uint32_t i = 0; i < w->swapchain_image_count; i++) {
        if (w->finished_blitting_sem && w->finished_blitting_sem[i])
            vkDestroySemaphore(dev, w->finished_blitting_sem[i], NULL);
    }

fail_alloc:
    free(w->image_available_sem);
    free(w->finished_blitting_sem);
    free(w->blit_fences);
    w->image_available_sem = NULL;
    w->finished_blitting_sem = NULL;
    w->blit_fences = NULL;
    return -1;
}

static int create_blit_cmds(window_res_t* w) {
    if (!w) return -1;

    VkDevice dev = g_device.logical_device;
    if (!dev) return -1;

    w->blit_cmds =
        (VkCommandBuffer*)malloc(sizeof(VkCommandBuffer) * FRAMES_IN_FLIGHT);
    if (!w->blit_cmds) return -1;

    memset(w->blit_cmds, 0, sizeof(VkCommandBuffer) * FRAMES_IN_FLIGHT);

    VkCommandPoolCreateInfo pool_ci = {
        .sType = VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO,
        .flags = VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT,
        .queueFamilyIndex = g_device.graphics_family,
    };

    if (vkCreateCommandPool(dev, &pool_ci, NULL, &w->blit_cmd_pool) !=
        VK_SUCCESS) {
        free(w->blit_cmds);
        w->blit_cmds = NULL;
        w->blit_cmd_pool = VK_NULL_HANDLE;
        return -1;
    }

    VkCommandBufferAllocateInfo alloc_ci = {
        .sType = VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO,
        .commandPool = w->blit_cmd_pool,
        .level = VK_COMMAND_BUFFER_LEVEL_PRIMARY,
        .commandBufferCount = FRAMES_IN_FLIGHT,
    };

    if (vkAllocateCommandBuffers(dev, &alloc_ci, w->blit_cmds) != VK_SUCCESS) {
        vkDestroyCommandPool(dev, w->blit_cmd_pool, NULL);
        w->blit_cmd_pool = VK_NULL_HANDLE;

        free(w->blit_cmds);
        w->blit_cmds = NULL;
        return -1;
    }

    return 0;
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
                                &w->swapchain) == VK_SUCCESS &&
           "failed to create swapchain");

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
                                 &w->swapchain_images[i].view) == VK_SUCCESS &&
               "failed to create swapchain image view");
    }

    free(images);
}

static void destroy_window(window_res_t* w) {
    if (!w) return;

    VkDevice dev = g_device.logical_device;

    vkDeviceWaitIdle(dev);

    for (uint32_t i = 0; i < FRAMES_IN_FLIGHT; i++) {
        if (w->blit_fences[i]) vkDestroyFence(dev, w->blit_fences[i], NULL);
    }

    for (uint32_t i = 0; i < FRAMES_IN_FLIGHT; i++) {
        if (w->finished_blitting_sem[i])
            vkDestroySemaphore(dev, w->finished_blitting_sem[i], NULL);
    }

    for (uint32_t i = 0; i < FRAMES_IN_FLIGHT; i++) {
        if (w->image_available_sem[i])
            vkDestroySemaphore(dev, w->image_available_sem[i], NULL);
    }

    if (w->blit_cmd_pool != VK_NULL_HANDLE) {
        if (w->blit_cmds) {
            vkFreeCommandBuffers(dev, w->blit_cmd_pool, FRAMES_IN_FLIGHT,
                                 w->blit_cmds);
        }

        vkDestroyCommandPool(dev, w->blit_cmd_pool, NULL);
        w->blit_cmd_pool = VK_NULL_HANDLE;
    }

    free(w->blit_cmds);
    w->blit_cmds = NULL;

    free(w->blit_fences);
    free(w->finished_blitting_sem);
    free(w->image_available_sem);

    w->blit_fences = NULL;
    w->finished_blitting_sem = NULL;
    w->image_available_sem = NULL;

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

static void window_res_dtor(ErlNifEnv* env, void* obj) {
    (void)env;

    vkDeviceWaitIdle(g_device.logical_device);

    destroy_window((window_res_t*)obj);
}

int nif_init_window_res(ErlNifEnv* env) {
    WINDOW_RES_TYPE =
        enif_open_resource_type(env, "fluo_nif", "window_res", window_res_dtor,
                                ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL);

    return WINDOW_RES_TYPE ? 0 : -1;
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

    if (create_sync_structs(res) != 0) {
        enif_release_resource(res);
        return enif_make_badarg(env);
    }

    if (create_blit_cmds(res) != 0) {
        enif_release_resource(res);
        return enif_make_badarg(env);
    }

    ERL_NIF_TERM handle_term = enif_make_resource(env, res);

    enif_release_resource(res);

    return handle_term;
}

ERL_NIF_TERM nif_window_should_close(ErlNifEnv* env, int argc,
                                     const ERL_NIF_TERM argv[]) {
    if (argc != 1) return enif_make_badarg(env);

    window_res_t* res = get_window_from_term(env, argv[0]);

    if (!res) return enif_make_badarg(env);

    glfwPollEvents();

    int should_close = glfwWindowShouldClose(res->handle);

    return should_close ? enif_make_atom(env, "true")
                        : enif_make_atom(env, "false");
}

ERL_NIF_TERM nif_window_keys_down(ErlNifEnv* env, int argc,
                                  const ERL_NIF_TERM argv[]) {
    if (argc != 1) return enif_make_badarg(env);

    window_res_t* w = get_window_from_term(env, argv[0]);
    if (!w || !w->handle) return enif_make_badarg(env);

    typedef struct {
        int glfw_key;
        const char* atom;
    } KeySpec;

    static const KeySpec ORDER[] = {
        {GLFW_KEY_ENTER, "enter"},
        {GLFW_KEY_ESCAPE, "escape"},
        {GLFW_KEY_SPACE, "space"},
        {GLFW_KEY_BACKSPACE, "backspace"},
        {GLFW_KEY_TAB, "tab"},

        {GLFW_KEY_LEFT_SHIFT, "l_shift"},
        {GLFW_KEY_RIGHT_SHIFT, "r_shift"},
        {GLFW_KEY_LEFT_ALT, "l_alt"},
        {GLFW_KEY_RIGHT_ALT, "r_alt"},
        {GLFW_KEY_LEFT_CONTROL, "l_ctrl"},
        {GLFW_KEY_RIGHT_CONTROL, "r_ctrl"},

        {GLFW_KEY_A, "key_a"},
        {GLFW_KEY_B, "key_b"},
        {GLFW_KEY_C, "key_c"},
        {GLFW_KEY_D, "key_d"},
        {GLFW_KEY_E, "key_e"},
        {GLFW_KEY_F, "key_f"},
        {GLFW_KEY_G, "key_g"},
        {GLFW_KEY_H, "key_h"},
        {GLFW_KEY_I, "key_i"},
        {GLFW_KEY_J, "key_j"},
        {GLFW_KEY_K, "key_k"},
        {GLFW_KEY_L, "key_l"},
        {GLFW_KEY_M, "key_m"},
        {GLFW_KEY_N, "key_n"},
        {GLFW_KEY_O, "key_o"},
        {GLFW_KEY_P, "key_p"},
        {GLFW_KEY_Q, "key_q"},
        {GLFW_KEY_R, "key_r"},
        {GLFW_KEY_S, "key_s"},
        {GLFW_KEY_T, "key_t"},
        {GLFW_KEY_U, "key_u"},
        {GLFW_KEY_V, "key_v"},
        {GLFW_KEY_W, "key_w"},
        {GLFW_KEY_X, "key_x"},
        {GLFW_KEY_Y, "key_y"},
        {GLFW_KEY_Z, "key_z"},

        {GLFW_KEY_UP, "arrow_up"},
        {GLFW_KEY_DOWN, "arrow_down"},
        {GLFW_KEY_LEFT, "arrow_left"},
        {GLFW_KEY_RIGHT, "arrow_right"},
    };

    ERL_NIF_TERM list = enif_make_list(env, 0);

    for (int i = (int)(sizeof(ORDER) / sizeof(ORDER[0])) - 1; i >= 0; --i) {
        if (glfwGetKey(w->handle, ORDER[i].glfw_key) == GLFW_PRESS) {
            list = enif_make_list_cell(env, enif_make_atom(env, ORDER[i].atom),
                                       list);
        }
    }

    return list;
}

ERL_NIF_TERM nif_window_mouse_pos(ErlNifEnv* env, int argc,
                                  const ERL_NIF_TERM argv[]) {
    if (argc != 1) return enif_make_badarg(env);

    window_res_t* w = get_window_from_term(env, argv[0]);
    if (!w || !w->handle) return enif_make_badarg(env);

    if (glfwGetInputMode(w->handle, GLFW_CURSOR) == GLFW_CURSOR_DISABLED) {
        return enif_make_tuple3(env, enif_make_atom(env, "position"),
                                enif_make_double(env, 0.0),
                                enif_make_double(env, 0.0));
    }

    return enif_make_tuple3(env, enif_make_atom(env, "position"),
                            enif_make_double(env, w->curr_cb_x),
                            enif_make_double(env, w->curr_cb_y));
}

ERL_NIF_TERM nif_window_mouse_delta(ErlNifEnv* env, int argc,
                                    const ERL_NIF_TERM argv[]) {
    if (argc != 1) return enif_make_badarg(env);

    window_res_t* w = get_window_from_term(env, argv[0]);
    if (!w || !w->handle) return enif_make_badarg(env);

    if (!glfwGetWindowAttrib(w->handle, GLFW_FOCUSED)) {
        reset_mouse_tracking(w);
        return enif_make_tuple3(env, enif_make_atom(env, "position"),
                                enif_make_double(env, 0.0),
                                enif_make_double(env, 0.0));
    }

    double dx = w->accum_dx;
    double dy = w->accum_dy;

    w->accum_dx = 0.0;
    w->accum_dy = 0.0;

    if (dx > 80.0) dx = 80.0;
    if (dx < -80.0) dx = -80.0;
    if (dy > 80.0) dy = 80.0;
    if (dy < -80.0) dy = -80.0;

    return enif_make_tuple3(env, enif_make_atom(env, "position"),
                            enif_make_double(env, dx),
                            enif_make_double(env, dy));
}

ERL_NIF_TERM nif_window_delta_time(ErlNifEnv* env, int argc,
                                   const ERL_NIF_TERM argv[]) {
    if (argc != 1) return enif_make_badarg(env);

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

ERL_NIF_TERM nif_window_capture_mouse(ErlNifEnv* env, int argc,
                                      const ERL_NIF_TERM argv[]) {
    if (argc != 1) return enif_make_badarg(env);

    window_res_t* w = get_window_from_term(env, argv[0]);
    if (!w || !w->handle) return enif_make_badarg(env);

    glfwSetInputMode(w->handle, GLFW_CURSOR, GLFW_CURSOR_DISABLED);

    reset_mouse_tracking(w);

    return enif_make_atom(env, "ok");
}

ERL_NIF_TERM nif_window_release_mouse(ErlNifEnv* env, int argc,
                                      const ERL_NIF_TERM argv[]) {
    if (argc != 1) return enif_make_badarg(env);

    window_res_t* w = get_window_from_term(env, argv[0]);
    if (!w || !w->handle) return enif_make_badarg(env);

    glfwSetInputMode(w->handle, GLFW_CURSOR, GLFW_CURSOR_NORMAL);

    reset_mouse_tracking(w);

    return enif_make_atom(env, "ok");
}

window_res_t* get_window_from_term(ErlNifEnv* env, ERL_NIF_TERM term) {
    const ERL_NIF_TERM* elems = NULL;
    int arity = 0;

    if (!enif_get_tuple(env, term, &arity, &elems)) return NULL;
    if (arity != 7) return NULL;

    ERL_NIF_TERM handle_term = elems[6];

    window_res_t* res = NULL;
    if (!enif_get_resource(env, handle_term, WINDOW_RES_TYPE, (void**)&res))
        return NULL;

    if (!res || !res->handle) return NULL;

    return res;
}

uint32_t get_curr_swapchain_idx(const window_res_t* w, VkSemaphore signal_sem) {
    if (!w) return UINT32_MAX;
    if (!w->swapchain) return UINT32_MAX;
    if (w->swapchain_image_count == 0 || !w->swapchain_images)
        return UINT32_MAX;
    if (signal_sem == VK_NULL_HANDLE) return UINT32_MAX;

    VkDevice dev = g_device.logical_device;
    if (!dev) return UINT32_MAX;

    uint32_t img_idx = 0;
    VkResult acq = vkAcquireNextImageKHR(dev, w->swapchain, UINT64_MAX,
                                         signal_sem, VK_NULL_HANDLE, &img_idx);

    if (acq == VK_ERROR_OUT_OF_DATE_KHR) return UINT32_MAX;

    if (acq != VK_SUCCESS && acq != VK_SUBOPTIMAL_KHR) {
        return UINT32_MAX;
    }

    if (img_idx >= w->swapchain_image_count) {
        return UINT32_MAX;
    }

    return img_idx;
}
