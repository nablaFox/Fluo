#include "window.h"

#include <GLFW/glfw3.h>
#include <stdlib.h>

#include "device.h"

typedef struct {
    struct Window* win;
} window_res_t;

static ErlNifResourceType* WINDOW_RES_TYPE = NULL;

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

static struct Window* create_window(const char* title, size_t width,
                                    size_t height) {
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

static void destroy_window(struct Window* window) {
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

static void window_res_dtor(ErlNifEnv* env, void* obj) {
    (void)env;
    window_res_t* r = (window_res_t*)obj;
    if (r->win) {
        destroy_window(r->win);
        r->win = NULL;
    }
}

ERL_NIF_TERM nif_create_window(ErlNifEnv* env, int argc,
                               const ERL_NIF_TERM argv[]) {
    if (argc != 3) return enif_make_badarg(env);

    int width, height;
    if (!enif_get_int(env, argv[1], &width)) return enif_make_badarg(env);
    if (!enif_get_int(env, argv[2], &height)) return enif_make_badarg(env);

    ErlNifBinary title_bin;
    if (!enif_inspect_binary(env, argv[0], &title_bin))
        return enif_make_badarg(env);

    char* title = strndup((char*)title_bin.data, title_bin.size);
    if (!title) return enif_make_badarg(env);

    struct Window* win = create_window(title, (size_t)width, (size_t)height);
    free(title);

    if (!win) return enif_make_badarg(env);

    window_res_t* res =
        enif_alloc_resource(WINDOW_RES_TYPE, sizeof(window_res_t));
    res->win = win;

    ERL_NIF_TERM handle_term = enif_make_resource(env, res);

    enif_release_resource(res);

    ERL_NIF_TERM width_term = enif_make_int(env, width);
    ERL_NIF_TERM height_term = enif_make_int(env, height);
    ERL_NIF_TERM title_term = argv[0];

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

    ERL_NIF_TERM tag = elems[0];

    if (!enif_is_atom(env, tag)) return enif_make_badarg(env);

    ERL_NIF_TERM handle_term = elems[6];

    window_res_t* res = NULL;
    if (!enif_get_resource(env, handle_term, WINDOW_RES_TYPE, (void**)&res)) {
        enif_fprintf(stderr, "Failed to get resource from handle=%T\n",
                     handle_term);
        return enif_make_badarg(env);
    }

    if (!res || !res->win) return enif_make_badarg(env);

    glfwPollEvents();
    int should_close = glfwWindowShouldClose(res->win->handle);

    return should_close ? enif_make_atom(env, "true")
                        : enif_make_atom(env, "false");
}

int nif_init_window_res(ErlNifEnv* env) {
    WINDOW_RES_TYPE =
        enif_open_resource_type(env, "fluo_nif", "window_res", NULL,
                                ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL);

    if (!WINDOW_RES_TYPE) return -1;

    return 0;
}
