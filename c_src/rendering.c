#include "rendering.h"

#include "device.h"
#include "mesh.h"
#include "renderer.h"
#include "utils.h"
#include "window.h"

static VkCommandBuffer render_cmds[FRAMES_IN_FLIGHT];
static VkFence render_fences[FRAMES_IN_FLIGHT];
static VkCommandPool g_render_cmd_pool = VK_NULL_HANDLE;
static uint32_t frame = FRAMES_IN_FLIGHT - 1;

static VkSemaphore finished_rendering_sem = VK_NULL_HANDLE;
static uint64_t render_timeline_value = 0;
static uint64_t frame_render_value[FRAMES_IN_FLIGHT] = {0};

static PFN_vkCmdBindShadersEXT vkCmdBindShadersEXT_;
static PFN_vkCmdSetVertexInputEXT vkCmdSetVertexInputEXT_;
static PFN_vkCmdSetPolygonModeEXT vkCmdSetPolygonModeEXT_;
static PFN_vkCmdSetRasterizationSamplesEXT vkCmdSetRasterizationSamplesEXT_;
static PFN_vkCmdSetSampleMaskEXT vkCmdSetSampleMaskEXT_;
static PFN_vkCmdSetAlphaToCoverageEnableEXT vkCmdSetAlphaToCoverageEnableEXT_;
static PFN_vkCmdSetAlphaToOneEnableEXT vkCmdSetAlphaToOneEnableEXT_;
static PFN_vkCmdSetColorBlendEnableEXT vkCmdSetColorBlendEnableEXT_;
static PFN_vkCmdSetColorWriteMaskEXT vkCmdSetColorWriteMaskEXT_;
static PFN_vkCmdSetColorBlendEquationEXT vkCmdSetColorBlendEquationEXT_;
static PFN_vkCmdSetLogicOpEnableEXT vkCmdSetLogicOpEnableEXT_;

int init_rendering_res() {
    memset(render_cmds, 0, sizeof(render_cmds));
    memset(render_fences, 0, sizeof(render_fences));
    memset(frame_render_value, 0, sizeof(frame_render_value));

    render_timeline_value = 0;

    VkDevice dev = g_device.logical_device;

    VkCommandPoolCreateInfo pool_info = {
        .sType = VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO,
        .pNext = NULL,
        .flags = VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT,
        .queueFamilyIndex = g_device.graphics_family,
    };

    if (vkCreateCommandPool(dev, &pool_info, NULL, &g_render_cmd_pool) !=
        VK_SUCCESS) {
        destroy_rendering_res();
        return -1;
    }

    VkCommandBufferAllocateInfo alloc_info = {
        .sType = VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO,
        .pNext = NULL,
        .commandPool = g_render_cmd_pool,
        .level = VK_COMMAND_BUFFER_LEVEL_PRIMARY,
        .commandBufferCount = FRAMES_IN_FLIGHT,
    };

    if (vkAllocateCommandBuffers(dev, &alloc_info, render_cmds) != VK_SUCCESS) {
        destroy_rendering_res();
        return -1;
    }

    VkSemaphoreTypeCreateInfo timeline_ci = {
        .sType = VK_STRUCTURE_TYPE_SEMAPHORE_TYPE_CREATE_INFO,
        .semaphoreType = VK_SEMAPHORE_TYPE_TIMELINE,
        .initialValue = 0,
    };

    VkSemaphoreCreateInfo sem_info = {
        .sType = VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO,
        .pNext = &timeline_ci,
    };

    if (vkCreateSemaphore(dev, &sem_info, NULL, &finished_rendering_sem) !=
        VK_SUCCESS) {
        destroy_rendering_res();
        return -1;
    }

    VkFenceCreateInfo fence_info = {0};
    fence_info.sType = VK_STRUCTURE_TYPE_FENCE_CREATE_INFO;
    fence_info.flags = VK_FENCE_CREATE_SIGNALED_BIT;

    for (uint32_t i = 0; i < FRAMES_IN_FLIGHT; i++) {
        if (vkCreateFence(dev, &fence_info, NULL, &render_fences[i]) !=
            VK_SUCCESS) {
            destroy_rendering_res();
            return -1;
        }
    }

    vkCmdBindShadersEXT_ = (PFN_vkCmdBindShadersEXT)vkGetDeviceProcAddr(
        dev, "vkCmdBindShadersEXT");
    vkCmdSetVertexInputEXT_ = (PFN_vkCmdSetVertexInputEXT)vkGetDeviceProcAddr(
        dev, "vkCmdSetVertexInputEXT");
    vkCmdSetPolygonModeEXT_ = (PFN_vkCmdSetPolygonModeEXT)vkGetDeviceProcAddr(
        dev, "vkCmdSetPolygonModeEXT");
    vkCmdSetRasterizationSamplesEXT_ =
        (PFN_vkCmdSetRasterizationSamplesEXT)vkGetDeviceProcAddr(
            dev, "vkCmdSetRasterizationSamplesEXT");
    vkCmdSetSampleMaskEXT_ = (PFN_vkCmdSetSampleMaskEXT)vkGetDeviceProcAddr(
        dev, "vkCmdSetSampleMaskEXT");
    vkCmdSetAlphaToCoverageEnableEXT_ =
        (PFN_vkCmdSetAlphaToCoverageEnableEXT)vkGetDeviceProcAddr(
            dev, "vkCmdSetAlphaToCoverageEnableEXT");
    vkCmdSetAlphaToOneEnableEXT_ =
        (PFN_vkCmdSetAlphaToOneEnableEXT)vkGetDeviceProcAddr(
            dev, "vkCmdSetAlphaToOneEnableEXT");
    vkCmdSetColorBlendEnableEXT_ =
        (PFN_vkCmdSetColorBlendEnableEXT)vkGetDeviceProcAddr(
            dev, "vkCmdSetColorBlendEnableEXT");
    vkCmdSetColorWriteMaskEXT_ =
        (PFN_vkCmdSetColorWriteMaskEXT)vkGetDeviceProcAddr(
            dev, "vkCmdSetColorWriteMaskEXT");
    vkCmdSetColorBlendEquationEXT_ =
        (PFN_vkCmdSetColorBlendEquationEXT)vkGetDeviceProcAddr(
            dev, "vkCmdSetColorBlendEquationEXT");
    vkCmdSetLogicOpEnableEXT_ =
        (PFN_vkCmdSetLogicOpEnableEXT)vkGetDeviceProcAddr(
            dev, "vkCmdSetLogicOpEnableEXT");

    if (!vkCmdBindShadersEXT_ || !vkCmdSetVertexInputEXT_ ||
        !vkCmdSetPolygonModeEXT_ || !vkCmdSetRasterizationSamplesEXT_ ||
        !vkCmdSetSampleMaskEXT_ || !vkCmdSetAlphaToCoverageEnableEXT_ ||
        !vkCmdSetAlphaToOneEnableEXT_ || !vkCmdSetColorBlendEnableEXT_ ||
        !vkCmdSetColorWriteMaskEXT_ || !vkCmdSetColorBlendEquationEXT_ ||
        !vkCmdSetLogicOpEnableEXT_) {
        return -1;
    }

    return 0;
}

void destroy_rendering_res() {
    VkDevice dev = g_device.logical_device;

    for (uint32_t i = 0; i < FRAMES_IN_FLIGHT; i++) {
        if (render_fences[i]) vkDestroyFence(dev, render_fences[i], NULL);

        render_fences[i] = VK_NULL_HANDLE;
        render_cmds[i] = VK_NULL_HANDLE;
    }

    if (finished_rendering_sem) {
        vkDestroySemaphore(dev, finished_rendering_sem, NULL);
        finished_rendering_sem = VK_NULL_HANDLE;
    }

    if (g_render_cmd_pool) {
        vkDestroyCommandPool(dev, g_render_cmd_pool, NULL);
        g_render_cmd_pool = VK_NULL_HANDLE;
    }
}

ERL_NIF_TERM nif_start_rendering(ErlNifEnv* env, int argc,
                                 const ERL_NIF_TERM argv[]) {
    frame = (frame + 1) % FRAMES_IN_FLIGHT;

    VkDevice dev = g_device.logical_device;

    vkWaitForFences(dev, 1, &render_fences[frame], VK_TRUE, UINT64_MAX);
    vkResetFences(dev, 1, &render_fences[frame]);

    VkCommandBuffer cmd = render_cmds[frame];
    vkResetCommandBuffer(cmd, 0);

    VkCommandBufferBeginInfo begin = {
        .sType = VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO,
    };

    THROW_VK_ERROR(env, vkBeginCommandBuffer(cmd, &begin));

    return enif_make_atom(env, "ok");
}

ERL_NIF_TERM nif_end_rendering(ErlNifEnv* env, int argc,
                               const ERL_NIF_TERM argv[]) {
    (void)argc;
    (void)argv;

    VkQueue q = g_device.graphics_queue;
    VkCommandBuffer cmd = render_cmds[frame];

    THROW_VK_ERROR(env, vkEndCommandBuffer(cmd));

    VkCommandBufferSubmitInfo cmd_info = {
        .sType = VK_STRUCTURE_TYPE_COMMAND_BUFFER_SUBMIT_INFO,
        .pNext = NULL,
        .commandBuffer = cmd,
        .deviceMask = 0,
    };

    uint64_t signal_value = ++render_timeline_value;
    frame_render_value[frame] = signal_value;

    VkSemaphoreSubmitInfo signal_info = {
        .sType = VK_STRUCTURE_TYPE_SEMAPHORE_SUBMIT_INFO,
        .semaphore = finished_rendering_sem,
        .value = signal_value,  // IMPORTANT
        .stageMask = VK_PIPELINE_STAGE_2_ALL_GRAPHICS_BIT,
        .deviceIndex = 0,
    };

    VkSubmitInfo2 submit = {
        .sType = VK_STRUCTURE_TYPE_SUBMIT_INFO_2,
        .commandBufferInfoCount = 1,
        .pCommandBufferInfos = &cmd_info,
        .signalSemaphoreInfoCount = 1,
        .pSignalSemaphoreInfos = &signal_info,
    };

    THROW_VK_ERROR(env, vkQueueSubmit2(q, 1, &submit, render_fences[frame]));

    return enif_make_atom(env, "ok");
}

ERL_NIF_TERM nif_swap_buffers(ErlNifEnv* env, int argc,
                              const ERL_NIF_TERM argv[]) {
    if (argc != 2) return enif_make_badarg(env);

    window_res_t* window = get_window_from_term(env, argv[0]);

    if (!window) return enif_make_badarg(env);

    image_res_t* color_image = get_image_from_term(env, argv[1]);

    if (!color_image) return enif_make_badarg(env);

    VkDevice dev = g_device.logical_device;

    VkCommandBuffer blit_cmd = window->blit_cmds[frame];

    vkWaitForFences(dev, 1, &window->blit_fences[frame], VK_TRUE, UINT64_MAX);
    vkResetFences(dev, 1, &window->blit_fences[frame]);

    uint32_t img_idx =
        get_curr_swapchain_idx(window, window->image_available_sem[frame]);

    if (img_idx == UINT32_MAX) return enif_make_atom(env, "out_of_date");

    vkResetCommandBuffer(blit_cmd, 0);

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

    blit_image(*color_image, *swapchain_image, blit_cmd);

    transition_iamge_to_optimal_layout(swapchain_image, blit_cmd);

    transition_iamge_to_optimal_layout(color_image, blit_cmd);

    THROW_VK_ERROR(env, vkEndCommandBuffer(blit_cmd));

    VkCommandBufferSubmitInfo cmd_info = {
        .sType = VK_STRUCTURE_TYPE_COMMAND_BUFFER_SUBMIT_INFO,
        .commandBuffer = blit_cmd,
        .deviceMask = 0,
    };

    VkSemaphoreSubmitInfo wait_sems[2] = {
        {
            .sType = VK_STRUCTURE_TYPE_SEMAPHORE_SUBMIT_INFO,
            .semaphore = finished_rendering_sem,
            .value = frame_render_value[frame],
            .stageMask = VK_PIPELINE_STAGE_2_TRANSFER_BIT,
        },
        {
            .sType = VK_STRUCTURE_TYPE_SEMAPHORE_SUBMIT_INFO,
            .semaphore = window->image_available_sem[frame],
            .value = 0,
            .stageMask = VK_PIPELINE_STAGE_2_TRANSFER_BIT,
        },
    };

    VkSemaphoreSubmitInfo signal_sem = {
        .sType = VK_STRUCTURE_TYPE_SEMAPHORE_SUBMIT_INFO,
        .semaphore = window->finished_blitting_sem[img_idx],
        .stageMask = VK_PIPELINE_STAGE_2_TRANSFER_BIT,
        .value = 0,
    };

    VkSubmitInfo2 submit = {
        .sType = VK_STRUCTURE_TYPE_SUBMIT_INFO_2,
        .waitSemaphoreInfoCount = 2,
        .pWaitSemaphoreInfos = wait_sems,
        .commandBufferInfoCount = 1,
        .pCommandBufferInfos = &cmd_info,
        .signalSemaphoreInfoCount = 1,
        .pSignalSemaphoreInfos = &signal_sem,
    };

    THROW_VK_ERROR(env, vkQueueSubmit2(g_device.graphics_queue, 1, &submit,
                                       window->blit_fences[frame]));

    VkPresentInfoKHR present = {
        .sType = VK_STRUCTURE_TYPE_PRESENT_INFO_KHR,
        .waitSemaphoreCount = 1,
        .pWaitSemaphores = &window->finished_blitting_sem[img_idx],
        .swapchainCount = 1,
        .pSwapchains = &window->swapchain,
        .pImageIndices = &img_idx,
    };

    VkResult pr = vkQueuePresentKHR(g_device.present_queue, &present);

    if (pr == VK_ERROR_OUT_OF_DATE_KHR)
        return enif_make_atom(env, "out_of_date");

    if (pr != VK_SUCCESS && pr != VK_SUBOPTIMAL_KHR) {
        THROW_VK_ERROR(env, pr);
    }

    return enif_make_atom(env, "ok");
}

static image_res_t* get_image_from_option(ErlNifEnv* env, ERL_NIF_TERM term) {
    if (enif_is_atom(env, term)) {
        char a[16];
        if (enif_get_atom(env, term, a, sizeof(a), ERL_NIF_LATIN1) &&
            strcmp(a, "none") == 0) {
            return NULL;
        }
    }

    const ERL_NIF_TERM* elems = NULL;
    int arity = 0;
    if (enif_get_tuple(env, term, &arity, &elems) && arity == 2) {
        char tag[16];
        if (enif_is_atom(env, elems[0]) &&
            enif_get_atom(env, elems[0], tag, sizeof(tag), ERL_NIF_LATIN1) &&
            strcmp(tag, "some") == 0) {
            return get_image_from_term(env, elems[1]);
        }
    }

    return get_image_from_term(env, term);
}

ERL_NIF_TERM nif_draw_mesh(ErlNifEnv* env, int argc,
                           const ERL_NIF_TERM argv[]) {
    const renderer_res_t* renderer = get_renderer_from_term(env, argv[0]);

    if (!renderer) return enif_make_badarg(env);

    const mesh_res_t* mesh = get_mesh_from_term(env, argv[1]);

    if (!mesh) return enif_make_badarg(env);

    const ERL_NIF_TERM params = argv[2];

    image_res_t* color_image = get_image_from_option(env, argv[3]);

    image_res_t* depth_image = get_image_from_option(env, argv[4]);

    if (!color_image && !depth_image) return enif_make_badarg(env);

    update_material_for_frame(env, renderer, frame, params);

    const VkClearColorValue clear_color = {{0.0f, 0.0f, 0.0f, 1.0f}};

    VkRenderingAttachmentInfo draw_attach_info = {
        .sType = VK_STRUCTURE_TYPE_RENDERING_ATTACHMENT_INFO,
        .imageLayout = VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
        .imageView = VK_NULL_HANDLE,
        .loadOp = VK_ATTACHMENT_LOAD_OP_CLEAR,
        .storeOp = VK_ATTACHMENT_STORE_OP_STORE,
        .clearValue = {.color = clear_color},
    };

    if (color_image != NULL) {
        draw_attach_info.imageView = color_image->view;
    }

    const VkClearDepthStencilValue clear_depth = {1.0f, 0};

    VkRenderingAttachmentInfo depth_attach_info = {
        .sType = VK_STRUCTURE_TYPE_RENDERING_ATTACHMENT_INFO,
        .imageLayout = VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL,
        .loadOp = VK_ATTACHMENT_LOAD_OP_CLEAR,
        .storeOp = VK_ATTACHMENT_STORE_OP_STORE,
        .clearValue.depthStencil = clear_depth,
    };

    if (depth_image != NULL) {
        depth_attach_info.imageView = depth_image->view;
    }

    VkExtent2D extent = {
        .width =
            color_image ? color_image->extent.width : depth_image->extent.width,
        .height = color_image ? color_image->extent.height
                              : depth_image->extent.height,
    };

    VkRenderingInfo rendering_info = {
        .sType = VK_STRUCTURE_TYPE_RENDERING_INFO,
        .renderArea = {{0, 0}, extent},
        .layerCount = 1,
    };

    VkCommandBuffer cmd = render_cmds[frame];

    if (color_image != NULL) {
        rendering_info.colorAttachmentCount = 1;
        rendering_info.pColorAttachments = &draw_attach_info;

        transition_image_layout((image_res_t*)color_image,
                                VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL, cmd);
    }

    if (depth_image != NULL) {
        rendering_info.pDepthAttachment = &depth_attach_info;

        transition_image_layout(
            (image_res_t*)depth_image,
            VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL, cmd);
    }

    vkCmdBeginRendering(cmd, &rendering_info);

    VkShaderStageFlagBits stages[] = {
        VK_SHADER_STAGE_VERTEX_BIT,
        VK_SHADER_STAGE_FRAGMENT_BIT,
    };

    VkShaderEXT shaders[2];

    shaders[0] = renderer->vert_shader;
    shaders[1] = renderer->frag_shader;

    vkCmdBindShadersEXT_(cmd, 2, stages, shaders);

    vkCmdBindDescriptorSets(cmd, VK_PIPELINE_BIND_POINT_GRAPHICS,
                            g_device.pipeline_layout, 0, 1,
                            &g_device.descriptor_set, 0, NULL);

    PushConstants push_constants = {
        .material_index = get_material_index_for_frame(renderer, frame),
    };

    vkCmdPushConstants(cmd, g_device.pipeline_layout, VK_SHADER_STAGE_ALL, 0,
                       sizeof(PushConstants), &push_constants);

    VkDeviceSize offset = 0;
    vkCmdBindVertexBuffers(cmd, 0, 1, &mesh->vertex_buffer.buffer, &offset);

    VkVertexInputBindingDescription2EXT binding = {
        .sType = VK_STRUCTURE_TYPE_VERTEX_INPUT_BINDING_DESCRIPTION_2_EXT,
        .binding = 0,
        .stride = sizeof(VertexGPU),
        .inputRate = VK_VERTEX_INPUT_RATE_VERTEX,
        .divisor = 1,
    };

    VkVertexInputAttributeDescription2EXT attributes[] = {
        {
            .sType = VK_STRUCTURE_TYPE_VERTEX_INPUT_ATTRIBUTE_DESCRIPTION_2_EXT,
            .location = 0,
            .binding = 0,
            .format = VK_FORMAT_R32G32B32_SFLOAT,
            .offset = offsetof(VertexGPU, pos),
        },
        {
            .sType = VK_STRUCTURE_TYPE_VERTEX_INPUT_ATTRIBUTE_DESCRIPTION_2_EXT,
            .location = 1,
            .binding = 0,
            .format = VK_FORMAT_R32G32B32_SFLOAT,
            .offset = offsetof(VertexGPU, normal),
        },
        {
            .sType = VK_STRUCTURE_TYPE_VERTEX_INPUT_ATTRIBUTE_DESCRIPTION_2_EXT,
            .location = 2,
            .binding = 0,
            .format = VK_FORMAT_R32G32_SFLOAT,
            .offset = offsetof(VertexGPU, uv),
        },
    };

    vkCmdSetVertexInputEXT_(cmd, 1, &binding, 3, attributes);

    VkDeviceSize offset_indices = 0;

    vkCmdBindIndexBuffer(cmd, mesh->index_buffer.buffer, offset_indices,
                         VK_INDEX_TYPE_UINT32);

    VkViewport viewport = {
        0, 0, (float)extent.width, (float)extent.height, 0.0f, 1.0f,
    };

    vkCmdSetViewportWithCount(cmd, 1, &viewport);
    VkRect2D scissor = {{0, 0}, extent};
    vkCmdSetScissorWithCount(cmd, 1, &scissor);
    vkCmdSetRasterizerDiscardEnable(cmd, VK_FALSE);
    vkCmdSetPolygonModeEXT_(cmd, VK_POLYGON_MODE_FILL);
    vkCmdSetRasterizationSamplesEXT_(cmd, VK_SAMPLE_COUNT_1_BIT);
    VkSampleMask sample_mask = 0xFFFFFFFF;
    vkCmdSetSampleMaskEXT_(cmd, VK_SAMPLE_COUNT_1_BIT, &sample_mask);
    vkCmdSetAlphaToCoverageEnableEXT_(cmd, VK_FALSE);
    vkCmdSetAlphaToOneEnableEXT_(cmd, VK_FALSE);
    vkCmdSetCullMode(cmd, VK_CULL_MODE_NONE);
    vkCmdSetFrontFace(cmd, VK_FRONT_FACE_CLOCKWISE);

    vkCmdSetDepthTestEnable(cmd, VK_TRUE);
    vkCmdSetDepthWriteEnable(cmd, VK_TRUE);
    vkCmdSetDepthCompareOp(cmd, VK_COMPARE_OP_LESS_OR_EQUAL);

    vkCmdSetDepthBiasEnable(cmd, VK_FALSE);
    vkCmdSetStencilTestEnable(cmd, VK_FALSE);
    vkCmdSetPrimitiveTopology(cmd, VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST);
    vkCmdSetPrimitiveRestartEnable(cmd, VK_FALSE);
    VkBool32 blend_enable = VK_FALSE;
    vkCmdSetColorBlendEnableEXT_(cmd, 0, 1, &blend_enable);
    VkColorComponentFlags write_mask =
        VK_COLOR_COMPONENT_R_BIT | VK_COLOR_COMPONENT_G_BIT |
        VK_COLOR_COMPONENT_B_BIT | VK_COLOR_COMPONENT_A_BIT;
    vkCmdSetColorWriteMaskEXT_(cmd, 0, 1, &write_mask);
    VkColorBlendEquationEXT blend_eq = {
        VK_BLEND_FACTOR_ONE, VK_BLEND_FACTOR_ZERO, VK_BLEND_OP_ADD,
        VK_BLEND_FACTOR_ONE, VK_BLEND_FACTOR_ZERO, VK_BLEND_OP_ADD,
    };
    vkCmdSetColorBlendEquationEXT_(cmd, 0, 1, &blend_eq);
    vkCmdSetLogicOpEnableEXT_(cmd, VK_FALSE);

    vkCmdDrawIndexed(cmd, mesh->indices_count, 1, 0, 0, 0);

    vkCmdEndRendering(cmd);

    return enif_make_atom(env, "ok");
}
