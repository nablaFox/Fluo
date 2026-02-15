#include "command.h"

#include "device.h"
#include "image.h"
#include "mesh.h"
#include "params.h"
#include "renderer.h"
#include "utils.h"

static VkCommandPool g_cmd_pool = VK_NULL_HANDLE;

static ErlNifResourceType* COMMAND_RES_TYPE = NULL;

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

static void command_res_dtor(ErlNifEnv* env, void* obj) {
    (void)env;

    command_res_t* res = (command_res_t*)obj;

    VkDevice dev = g_device.logical_device;

    vkDeviceWaitIdle(dev);

    for (uint32_t i = 0; i < FRAMES_IN_FLIGHT; i++) {
        if (res->fences[i]) vkDestroyFence(dev, res->fences[i], NULL);

        if (res->cmds[i])
            vkFreeCommandBuffers(dev, g_cmd_pool, 1, &res->cmds[i]);

        if (res->finished_sem[i])
            vkDestroySemaphore(dev, res->finished_sem[i], NULL);

        res->fences[i] = VK_NULL_HANDLE;
        res->cmds[i] = VK_NULL_HANDLE;
    }
}

void destroy_command_pool() {
    if (g_cmd_pool == VK_NULL_HANDLE) return;

    vkDestroyCommandPool(g_device.logical_device, g_cmd_pool, NULL);
    g_cmd_pool = VK_NULL_HANDLE;
}

int nif_init_command_res(ErlNifEnv* env) {
    VkDevice dev = g_device.logical_device;

    if (g_cmd_pool == VK_NULL_HANDLE) {
        VkCommandPoolCreateInfo pool_ci = {
            .sType = VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO,
            .flags = VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT,
            .queueFamilyIndex = g_device.graphics_family,
        };

        if (vkCreateCommandPool(dev, &pool_ci, NULL, &g_cmd_pool) !=
            VK_SUCCESS) {
            g_cmd_pool = VK_NULL_HANDLE;
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

    COMMAND_RES_TYPE = enif_open_resource_type(
        env, "fluo_nif", "command_res", command_res_dtor,
        ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL);

    return 0;
}

ERL_NIF_TERM nif_create_command(ErlNifEnv* env, int argc,
                                const ERL_NIF_TERM argv[]) {
    if (argc != 0) return enif_make_badarg(env);

    VkDevice dev = g_device.logical_device;

    VkCommandBufferAllocateInfo alloc_info = {
        .sType = VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO,
        .pNext = NULL,
        .commandPool = g_cmd_pool,
        .level = VK_COMMAND_BUFFER_LEVEL_PRIMARY,
        .commandBufferCount = FRAMES_IN_FLIGHT,
    };

    command_res_t* res =
        enif_alloc_resource(COMMAND_RES_TYPE, sizeof(command_res_t));

    res->frame = 0;
    res->last_submitted_frame = 0;

    if (vkAllocateCommandBuffers(dev, &alloc_info, res->cmds) != VK_SUCCESS) {
        enif_release_resource(res);
        return enif_make_badarg(env);
    }

    VkFenceCreateInfo fence_info = {
        .sType = VK_STRUCTURE_TYPE_FENCE_CREATE_INFO,
        .flags = VK_FENCE_CREATE_SIGNALED_BIT,
    };

    VkSemaphoreCreateInfo sem_info = {
        .sType = VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO,
        .pNext = NULL,
        .flags = 0,
    };

    for (uint32_t i = 0; i < FRAMES_IN_FLIGHT; i++) {
        if (vkCreateFence(dev, &fence_info, NULL, &res->fences[i]) !=
            VK_SUCCESS) {
            enif_release_resource(res);
            return enif_make_badarg(env);
        }

        if (vkCreateSemaphore(dev, &sem_info, NULL, &res->finished_sem[i]) !=
            VK_SUCCESS) {
            enif_release_resource(res);
            return enif_make_badarg(env);
        }
    }

    ERL_NIF_TERM term = enif_make_resource(env, res);

    enif_release_resource(res);

    return term;
}

command_res_t* get_command_from_term(ErlNifEnv* env, ERL_NIF_TERM term) {
    command_res_t* res = NULL;

    if (!enif_get_resource(env, term, COMMAND_RES_TYPE, (void**)&res))
        return NULL;

    if (!res) return NULL;

    return res;
}

ERL_NIF_TERM nif_start_command_recording(ErlNifEnv* env, int argc,
                                         const ERL_NIF_TERM argv[]) {
    enif_mutex_lock(g_vk_mutex);

    command_res_t* cmd_res = get_command_from_term(env, argv[0]);
    if (!cmd_res) return enif_make_badarg(env);

    uint32_t frame = cmd_res->frame;

    vkWaitForFences(g_device.logical_device, 1, &cmd_res->fences[frame],
                    VK_TRUE, UINT64_MAX);

    vkResetFences(g_device.logical_device, 1, &cmd_res->fences[frame]);

    VkCommandBuffer cmd = cmd_res->cmds[frame];
    vkResetCommandBuffer(cmd, 0);

    VkCommandBufferBeginInfo begin = {
        .sType = VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO,
    };

    THROW_VK_ERROR(env, vkBeginCommandBuffer(cmd, &begin));

    enif_mutex_unlock(g_vk_mutex);

    return enif_make_atom(env, "ok");
}

ERL_NIF_TERM nif_end_command_recording(ErlNifEnv* env, int argc,
                                       const ERL_NIF_TERM argv[]) {
    enif_mutex_lock(g_vk_mutex);

    const command_res_t* cmd_res = get_command_from_term(env, argv[0]);

    if (!cmd_res) return enif_make_badarg(env);

    THROW_VK_ERROR(env, vkEndCommandBuffer(cmd_res->cmds[cmd_res->frame]));

    enif_mutex_unlock(g_vk_mutex);

    return enif_make_atom(env, "ok");
}

ERL_NIF_TERM nif_submit_command(ErlNifEnv* env, int argc,
                                const ERL_NIF_TERM argv[]) {
    enif_mutex_lock(g_vk_mutex);

    command_res_t* cmd_res = get_command_from_term(env, argv[0]);

    if (!cmd_res) return enif_make_badarg(env);

    const uint32_t frame = cmd_res->frame;

    VkCommandBuffer cmd = cmd_res->cmds[frame];

    VkCommandBufferSubmitInfo cmd_info = {
        .sType = VK_STRUCTURE_TYPE_COMMAND_BUFFER_SUBMIT_INFO,
        .pNext = NULL,
        .commandBuffer = cmd,
        .deviceMask = 0,
    };

    VkSemaphoreSubmitInfo signal_info = {
        .sType = VK_STRUCTURE_TYPE_SEMAPHORE_SUBMIT_INFO,
        .value = 0,
        .semaphore = cmd_res->finished_sem[frame],
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

    THROW_VK_ERROR(env, vkQueueSubmit2(g_device.graphics_queue, 1, &submit,
                                       cmd_res->fences[frame]));

    cmd_res->last_submitted_frame = frame;
    cmd_res->frame = (frame + 1) % FRAMES_IN_FLIGHT;

    enif_mutex_unlock(g_vk_mutex);

    return enif_make_atom(env, "ok");
}

ERL_NIF_TERM nif_start_rendering(ErlNifEnv* env, int argc,
                                 const ERL_NIF_TERM argv[]) {
    const command_res_t* cmd_res = get_command_from_term(env, argv[0]);

    if (!cmd_res) return enif_make_badarg(env);

    VkCommandBuffer cmd = cmd_res->cmds[cmd_res->frame];

    image_res_t* color_image = get_image_from_option(env, argv[1]);

    image_res_t* depth_image = get_image_from_option(env, argv[2]);

    if (!color_image && !depth_image) return enif_make_badarg(env);

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

    return enif_make_atom(env, "ok");
}

ERL_NIF_TERM nif_end_rendering(ErlNifEnv* env, int argc,
                               const ERL_NIF_TERM argv[]) {
    enif_mutex_lock(g_vk_mutex);

    const command_res_t* cmd_res = get_command_from_term(env, argv[0]);

    if (!cmd_res) return enif_make_badarg(env);

    VkCommandBuffer cmd = cmd_res->cmds[cmd_res->frame];

    vkCmdEndRendering(cmd);

    enif_mutex_unlock(g_vk_mutex);

    return enif_make_atom(env, "ok");
}

ERL_NIF_TERM nif_draw_mesh(ErlNifEnv* env, int argc,
                           const ERL_NIF_TERM argv[]) {
    enif_mutex_lock(g_vk_mutex);

    const command_res_t* cmd_res = get_command_from_term(env, argv[0]);

    if (!cmd_res) return enif_make_badarg(env);

    const renderer_res_t* renderer = get_renderer_from_term(env, argv[1]);

    if (!renderer) return enif_make_badarg(env);

    const mesh_res_t* mesh = get_mesh_from_term(env, argv[2]);

    if (!mesh) return enif_make_badarg(env);

    const ERL_NIF_TERM params = argv[3];
    // TODO: use params

    VkViewport viewport = {0};

    if (!get_viewport_from_term(env, argv[4], &viewport))
        return enif_make_badarg(env);

    VkRect2D scissor = {0};

    if (!get_scissor_from_term(env, argv[5], &scissor))
        return enif_make_badarg(env);

    VkCommandBuffer cmd = cmd_res->cmds[cmd_res->frame];

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
        .material_index = renderer->material_index,
        .frame_params_index = get_frame_params_index(renderer, cmd_res->frame),
        // TODO: add per draw params
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

    vkCmdSetViewportWithCount(cmd, 1, &viewport);
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

    enif_mutex_unlock(g_vk_mutex);

    return enif_make_atom(env, "ok");
}
