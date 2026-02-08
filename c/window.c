#include "window.h"

#include "device.h"
#include "utils.h"

static ErlNifResourceType* WINDOW_RES_TYPE = NULL;

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
    assert(count > 0);

    VkPresentModeKHR* modes =
        (VkPresentModeKHR*)malloc(count * sizeof(VkPresentModeKHR));
    assert(modes);

    vkGetPhysicalDeviceSurfacePresentModesKHR(g_device.physical_device, surface,
                                              &count, modes);

    VkPresentModeKHR chosen = VK_PRESENT_MODE_FIFO_KHR;

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
        ci.imageSharingMode = VK_SHARING_MODE_CONCURRENT;
        ci.queueFamilyIndexCount = 2;
        ci.pQueueFamilyIndices = queue_families;
    } else {
        ci.imageSharingMode = VK_SHARING_MODE_EXCLUSIVE;
    }

    VkResult r =
        vkCreateSwapchainKHR(g_device.logical_device, &ci, NULL, &w->swapchain);
    assert(r == VK_SUCCESS && "failed to create swapchain");

    w->swapchain_format = surface_format.format;
    w->swapchain_extent = extent;
}

static void get_swapchain_images(window_res_t* w) {
    vkGetSwapchainImagesKHR(g_device.logical_device, w->swapchain,
                            &w->image_count, NULL);
    assert(w->image_count > 0);

    w->images = (VkImage*)malloc(w->image_count * sizeof(VkImage));
    assert(w->images);

    vkGetSwapchainImagesKHR(g_device.logical_device, w->swapchain,
                            &w->image_count, w->images);
}

static void create_image_views(window_res_t* w) {
    w->image_views = (VkImageView*)malloc(w->image_count * sizeof(VkImageView));
    assert(w->image_views);

    for (uint32_t i = 0; i < w->image_count; i++) {
        VkImageViewCreateInfo ci = {
            .sType = VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO,
            .image = w->images[i],
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

        VkResult r = vkCreateImageView(g_device.logical_device, &ci, NULL,
                                       &w->image_views[i]);
        assert(r == VK_SUCCESS && "failed to create image view");
    }
}

static void destroy_window(window_res_t* w) {
    if (!w) return;

    if (w->image_views) {
        for (uint32_t i = 0; i < w->image_count; i++) {
            if (w->image_views[i]) {
                vkDestroyImageView(g_device.logical_device, w->image_views[i],
                                   NULL);
                w->image_views[i] = VK_NULL_HANDLE;
            }
        }
        free(w->image_views);
        w->image_views = NULL;
    }

    if (w->images) {
        free(w->images);
        w->images = NULL;
    }

    w->image_count = 0;

    if (w->swapchain) {
        vkDestroySwapchainKHR(g_device.logical_device, w->swapchain, NULL);
        w->swapchain = VK_NULL_HANDLE;
    }

    if (w->surface) {
        vkDestroySurfaceKHR(g_device.instance, w->surface, NULL);
        w->surface = VK_NULL_HANDLE;
    }

    if (w->handle) {
        glfwDestroyWindow(w->handle);
        w->handle = NULL;
    }
}

static void window_res_dtor(ErlNifEnv* env, void* obj) {
    (void)env;
    destroy_window((window_res_t*)obj);
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

    res->handle = glfwCreateWindow(width, height, title, NULL, NULL);

    free(title);

    if (!res->handle) {
        enif_release_resource(res);
        return enif_make_badarg(env);
    }

    VkResult vr = glfwCreateWindowSurface(g_device.instance, res->handle, NULL,
                                          &res->surface);
    if (vr != VK_SUCCESS) {
        enif_release_resource(res);
        return enif_make_badarg(env);
    }

    create_swapchain(res);
    get_swapchain_images(res);
    create_image_views(res);

    ERL_NIF_TERM handle_term = enif_make_resource(env, res);
    enif_release_resource(res);

    ERL_NIF_TERM color_term = enif_make_tuple2(env, enif_make_int(env, width),
                                               enif_make_int(env, height));

    ERL_NIF_TERM depth_term = enif_make_tuple2(env, enif_make_int(env, width),
                                               enif_make_int(env, height));

    return enif_make_tuple3(env, color_term, depth_term, handle_term);
}

ERL_NIF_TERM nif_window_should_close(ErlNifEnv* env, int argc,
                                     const ERL_NIF_TERM argv[]) {
    if (argc != 1) return enif_make_badarg(env);

    const ERL_NIF_TERM* elems = NULL;
    int arity = 0;

    if (!enif_get_tuple(env, argv[0], &arity, &elems))
        return enif_make_badarg(env);

    if (arity != 7) return enif_make_badarg(env);

    ERL_NIF_TERM handle_term = elems[6];

    window_res_t* res = NULL;
    if (!enif_get_resource(env, handle_term, WINDOW_RES_TYPE, (void**)&res))
        return enif_make_badarg(env);

    if (!res || !res->handle) return enif_make_badarg(env);

    glfwPollEvents();
    int should_close = glfwWindowShouldClose(res->handle);

    return should_close ? enif_make_atom(env, "true")
                        : enif_make_atom(env, "false");
}

int nif_init_window_res(ErlNifEnv* env) {
    WINDOW_RES_TYPE =
        enif_open_resource_type(env, "fluo_nif", "window_res", window_res_dtor,
                                ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL);

    return WINDOW_RES_TYPE ? 0 : -1;
}
