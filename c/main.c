#include <assert.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>

#include "device.h"
#include "window.h"

static PFN_vkCreateShadersEXT vkCreateShadersEXT_;
static PFN_vkDestroyShaderEXT vkDestroyShaderEXT_;
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

static uint8_t* read_file(const char* path, size_t* size) {
    FILE* f = fopen(path, "rb");
    assert(f && "failed to open shader file");
    fseek(f, 0, SEEK_END);
    *size = (size_t)ftell(f);
    fseek(f, 0, SEEK_SET);
    uint8_t* data = (uint8_t*)malloc(*size);
    fread(data, 1, *size, f);
    fclose(f);
    return data;
}

struct Vertex {
    float pos[2];
    float color[3];
};

struct Material {
    float alpha;
};

static struct Vertex vertices[] = {
    {{0.0f, -0.5f}, {1.0f, 0.0f, 0.0f}},
    {{0.5f, 0.5f}, {0.0f, 1.0f, 0.0f}},
    {{-0.5f, 0.5f}, {0.0f, 0.0f, 1.0f}},
};

#define MAX_FRAMES 2

int main(int argc, char* argv[]) {
    VkResult r;

    init_device();
    VkDevice dev = g_device.logical_device;

#define LOAD(fn)                                     \
    fn##_ = (PFN_##fn)vkGetDeviceProcAddr(dev, #fn); \
    assert(fn##_);
    LOAD(vkCreateShadersEXT);
    LOAD(vkDestroyShaderEXT);
    LOAD(vkCmdBindShadersEXT);
    LOAD(vkCmdSetVertexInputEXT);
    LOAD(vkCmdSetPolygonModeEXT);
    LOAD(vkCmdSetRasterizationSamplesEXT);
    LOAD(vkCmdSetSampleMaskEXT);
    LOAD(vkCmdSetAlphaToCoverageEnableEXT);
    LOAD(vkCmdSetAlphaToOneEnableEXT);
    LOAD(vkCmdSetColorBlendEnableEXT);
    LOAD(vkCmdSetColorWriteMaskEXT);
    LOAD(vkCmdSetColorBlendEquationEXT);
    LOAD(vkCmdSetLogicOpEnableEXT);
#undef LOAD

    struct Window* window = create_window("Vulkan Triangle", 800, 600);

    VkBuffer vertex_buffer;
    VmaAllocation vertex_alloc;

    VkBufferCreateInfo vb_info = {
        .sType = VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO,
        .size = sizeof(vertices),
        .usage = VK_BUFFER_USAGE_VERTEX_BUFFER_BIT,
    };

    VmaAllocationCreateInfo vb_alloc_info = {
        .usage = VMA_MEMORY_USAGE_AUTO,
        .flags = VMA_ALLOCATION_CREATE_HOST_ACCESS_SEQUENTIAL_WRITE_BIT,
    };

    vmaCreateBuffer(g_device.allocator, &vb_info, &vb_alloc_info,
                    &vertex_buffer, &vertex_alloc, NULL);
    void* mapped;
    vmaMapMemory(g_device.allocator, vertex_alloc, &mapped);
    memcpy(mapped, vertices, sizeof(vertices));
    vmaUnmapMemory(g_device.allocator, vertex_alloc);

    struct Material material = {.alpha = 0.5f};

    VkBuffer material_buffer;
    VmaAllocation material_alloc;

    VkBufferCreateInfo mat_info = {
        .sType = VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO,
        .size = sizeof(struct Material),
        .usage = VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT,
    };

    VmaAllocationCreateInfo mat_alloc_info = {
        .usage = VMA_MEMORY_USAGE_AUTO,
        .flags = VMA_ALLOCATION_CREATE_HOST_ACCESS_SEQUENTIAL_WRITE_BIT,
    };

    vmaCreateBuffer(g_device.allocator, &mat_info, &mat_alloc_info,
                    &material_buffer, &material_alloc, NULL);
    vmaMapMemory(g_device.allocator, material_alloc, &mapped);
    memcpy(mapped, &material, sizeof(material));
    vmaUnmapMemory(g_device.allocator, material_alloc);

    VkDescriptorBufferInfo desc_buf_info = {
        .buffer = material_buffer,
        .offset = 0,
        .range = sizeof(struct Material),
    };

    VkWriteDescriptorSet write = {
        .sType = VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET,
        .dstSet = g_device.descriptor_set,
        .dstBinding = UNIFORM_BINDING,
        .dstArrayElement = 0,  // material index 0
        .descriptorCount = 1,
        .descriptorType = VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,
        .pBufferInfo = &desc_buf_info,
    };

    vkUpdateDescriptorSets(dev, 1, &write, 0, NULL);

    size_t vert_size, frag_size;
    uint8_t* vert_code = read_file("vert.spv", &vert_size);
    uint8_t* frag_code = read_file("frag.spv", &frag_size);

    VkPushConstantRange push_range = {
        .stageFlags = VK_SHADER_STAGE_ALL,
        .offset = 0,
        .size = sizeof(uint32_t),
    };

    VkShaderCreateInfoEXT shader_infos[2] = {
        {
            .sType = VK_STRUCTURE_TYPE_SHADER_CREATE_INFO_EXT,
            .stage = VK_SHADER_STAGE_VERTEX_BIT,
            .nextStage = VK_SHADER_STAGE_FRAGMENT_BIT,
            .codeType = VK_SHADER_CODE_TYPE_SPIRV_EXT,
            .codeSize = vert_size,
            .pCode = vert_code,
            .pName = "main",
            .setLayoutCount = 1,
            .pSetLayouts = &g_device.descriptor_layout,
            .pushConstantRangeCount = 1,
            .pPushConstantRanges = &push_range,
        },
        {
            .sType = VK_STRUCTURE_TYPE_SHADER_CREATE_INFO_EXT,
            .stage = VK_SHADER_STAGE_FRAGMENT_BIT,
            .nextStage = 0,
            .codeType = VK_SHADER_CODE_TYPE_SPIRV_EXT,
            .codeSize = frag_size,
            .pCode = frag_code,
            .pName = "main",
            .setLayoutCount = 1,
            .pSetLayouts = &g_device.descriptor_layout,
            .pushConstantRangeCount = 1,
            .pPushConstantRanges = &push_range,
        },
    };

    VkShaderEXT shaders[2];
    r = vkCreateShadersEXT_(dev, 2, shader_infos, NULL, shaders);
    assert(r == VK_SUCCESS && "failed to create shader objects");
    free(vert_code);
    free(frag_code);

    VkCommandPool cmd_pool;
    VkCommandPoolCreateInfo pool_info = {
        .sType = VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO,
        .flags = VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT,
        .queueFamilyIndex = g_device.graphics_family,
    };

    vkCreateCommandPool(dev, &pool_info, NULL, &cmd_pool);

    VkCommandBuffer cmds[MAX_FRAMES];
    VkSemaphore sem_available[MAX_FRAMES];
    VkFence fences[MAX_FRAMES];

    VkCommandBufferAllocateInfo alloc = {
        .sType = VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO,
        .commandPool = cmd_pool,
        .level = VK_COMMAND_BUFFER_LEVEL_PRIMARY,
        .commandBufferCount = MAX_FRAMES,
    };

    vkAllocateCommandBuffers(dev, &alloc, cmds);

    VkSemaphoreCreateInfo sem_ci = {
        .sType = VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO};
    VkFenceCreateInfo fence_ci = {
        .sType = VK_STRUCTURE_TYPE_FENCE_CREATE_INFO,
        .flags = VK_FENCE_CREATE_SIGNALED_BIT,
    };
    for (int i = 0; i < MAX_FRAMES; i++) {
        vkCreateSemaphore(dev, &sem_ci, NULL, &sem_available[i]);
        vkCreateFence(dev, &fence_ci, NULL, &fences[i]);
    }

    VkSemaphore* sem_finished =
        (VkSemaphore*)malloc(window->image_count * sizeof(VkSemaphore));
    for (uint32_t i = 0; i < window->image_count; i++) {
        vkCreateSemaphore(dev, &sem_ci, NULL, &sem_finished[i]);
    }

    uint32_t frame = 0;

    while (!window_should_close(window)) {
        vkWaitForFences(dev, 1, &fences[frame], VK_TRUE, UINT64_MAX);
        vkResetFences(dev, 1, &fences[frame]);

        uint32_t img_idx;
        vkAcquireNextImageKHR(dev, window->swapchain, UINT64_MAX,
                              sem_available[frame], VK_NULL_HANDLE, &img_idx);

        VkCommandBuffer cmd = cmds[frame];
        vkResetCommandBuffer(cmd, 0);

        VkCommandBufferBeginInfo begin = {
            .sType = VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO,
        };

        vkBeginCommandBuffer(cmd, &begin);

        VkImageMemoryBarrier2 barrier_to_attach = {
            .sType = VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER_2,
            .srcStageMask = VK_PIPELINE_STAGE_2_TOP_OF_PIPE_BIT,
            .srcAccessMask = 0,
            .dstStageMask = VK_PIPELINE_STAGE_2_COLOR_ATTACHMENT_OUTPUT_BIT,
            .dstAccessMask = VK_ACCESS_2_COLOR_ATTACHMENT_WRITE_BIT,
            .oldLayout = VK_IMAGE_LAYOUT_UNDEFINED,
            .newLayout = VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
            .srcQueueFamilyIndex = VK_QUEUE_FAMILY_IGNORED,
            .dstQueueFamilyIndex = VK_QUEUE_FAMILY_IGNORED,
            .image = window->images[img_idx],
            .subresourceRange = {VK_IMAGE_ASPECT_COLOR_BIT, 0, 1, 0, 1},
        };

        VkDependencyInfo dep1 = {
            .sType = VK_STRUCTURE_TYPE_DEPENDENCY_INFO,
            .imageMemoryBarrierCount = 1,
            .pImageMemoryBarriers = &barrier_to_attach,
        };

        vkCmdPipelineBarrier2(cmd, &dep1);

        VkRenderingAttachmentInfo color_att = {
            .sType = VK_STRUCTURE_TYPE_RENDERING_ATTACHMENT_INFO,
            .imageView = window->image_views[img_idx],
            .imageLayout = VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
            .loadOp = VK_ATTACHMENT_LOAD_OP_CLEAR,
            .storeOp = VK_ATTACHMENT_STORE_OP_STORE,
            .clearValue = {.color = {{0.0f, 0.0f, 0.0f, 1.0f}}},
        };

        VkRenderingInfo render_info = {
            .sType = VK_STRUCTURE_TYPE_RENDERING_INFO,
            .renderArea = {{0, 0}, window->swapchain_extent},
            .layerCount = 1,
            .colorAttachmentCount = 1,
            .pColorAttachments = &color_att,
        };

        vkCmdBeginRendering(cmd, &render_info);

        VkShaderStageFlagBits stages[] = {
            VK_SHADER_STAGE_VERTEX_BIT,
            VK_SHADER_STAGE_FRAGMENT_BIT,
        };

        vkCmdBindShadersEXT_(cmd, 2, stages, shaders);

        vkCmdBindDescriptorSets(cmd, VK_PIPELINE_BIND_POINT_GRAPHICS,
                                g_device.pipeline_layout, 0, 1,
                                &g_device.descriptor_set, 0, NULL);

        uint32_t material_index = 0;
        vkCmdPushConstants(cmd, g_device.pipeline_layout, VK_SHADER_STAGE_ALL,
                           0, sizeof(uint32_t), &material_index);

        VkDeviceSize offset = 0;
        vkCmdBindVertexBuffers(cmd, 0, 1, &vertex_buffer, &offset);

        VkVertexInputBindingDescription2EXT binding = {
            .sType = VK_STRUCTURE_TYPE_VERTEX_INPUT_BINDING_DESCRIPTION_2_EXT,
            .binding = 0,
            .stride = sizeof(struct Vertex),
            .inputRate = VK_VERTEX_INPUT_RATE_VERTEX,
            .divisor = 1,
        };

        VkVertexInputAttributeDescription2EXT attributes[] = {
            {
                .sType =
                    VK_STRUCTURE_TYPE_VERTEX_INPUT_ATTRIBUTE_DESCRIPTION_2_EXT,
                .location = 0,
                .binding = 0,
                .format = VK_FORMAT_R32G32_SFLOAT,
                .offset = offsetof(struct Vertex, pos),
            },
            {
                .sType =
                    VK_STRUCTURE_TYPE_VERTEX_INPUT_ATTRIBUTE_DESCRIPTION_2_EXT,
                .location = 1,
                .binding = 0,
                .format = VK_FORMAT_R32G32B32_SFLOAT,
                .offset = offsetof(struct Vertex, color),
            },
        };

        vkCmdSetVertexInputEXT_(cmd, 1, &binding, 2, attributes);

        VkViewport viewport = {
            0,
            0,
            (float)window->swapchain_extent.width,
            (float)window->swapchain_extent.height,
            0.0f,
            1.0f,
        };

        vkCmdSetViewportWithCount(cmd, 1, &viewport);
        VkRect2D scissor = {{0, 0}, window->swapchain_extent};
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
        vkCmdSetDepthTestEnable(cmd, VK_FALSE);
        vkCmdSetDepthWriteEnable(cmd, VK_FALSE);
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

        vkCmdDraw(cmd, 3, 1, 0, 0);

        vkCmdEndRendering(cmd);

        VkImageMemoryBarrier2 barrier_to_present = {
            .sType = VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER_2,
            .srcStageMask = VK_PIPELINE_STAGE_2_COLOR_ATTACHMENT_OUTPUT_BIT,
            .srcAccessMask = VK_ACCESS_2_COLOR_ATTACHMENT_WRITE_BIT,
            .dstStageMask = VK_PIPELINE_STAGE_2_BOTTOM_OF_PIPE_BIT,
            .dstAccessMask = 0,
            .oldLayout = VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
            .newLayout = VK_IMAGE_LAYOUT_PRESENT_SRC_KHR,
            .srcQueueFamilyIndex = VK_QUEUE_FAMILY_IGNORED,
            .dstQueueFamilyIndex = VK_QUEUE_FAMILY_IGNORED,
            .image = window->images[img_idx],
            .subresourceRange = {VK_IMAGE_ASPECT_COLOR_BIT, 0, 1, 0, 1},
        };

        VkDependencyInfo dep2 = {
            .sType = VK_STRUCTURE_TYPE_DEPENDENCY_INFO,
            .imageMemoryBarrierCount = 1,
            .pImageMemoryBarriers = &barrier_to_present,
        };

        vkCmdPipelineBarrier2(cmd, &dep2);

        vkEndCommandBuffer(cmd);

        VkPipelineStageFlags wait_stage =
            VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT;
        VkSubmitInfo submit = {
            .sType = VK_STRUCTURE_TYPE_SUBMIT_INFO,
            .waitSemaphoreCount = 1,
            .pWaitSemaphores = &sem_available[frame],
            .pWaitDstStageMask = &wait_stage,
            .commandBufferCount = 1,
            .pCommandBuffers = &cmd,
            .signalSemaphoreCount = 1,
            .pSignalSemaphores = &sem_finished[img_idx],
        };

        vkQueueSubmit(g_device.graphics_queue, 1, &submit, fences[frame]);

        VkPresentInfoKHR present = {
            .sType = VK_STRUCTURE_TYPE_PRESENT_INFO_KHR,
            .waitSemaphoreCount = 1,
            .pWaitSemaphores = &sem_finished[img_idx],
            .swapchainCount = 1,
            .pSwapchains = &window->swapchain,
            .pImageIndices = &img_idx,
        };

        vkQueuePresentKHR(g_device.present_queue, &present);

        frame = (frame + 1) % MAX_FRAMES;
    }

    vkDeviceWaitIdle(dev);

    vmaDestroyBuffer(g_device.allocator, vertex_buffer, vertex_alloc);
    vmaDestroyBuffer(g_device.allocator, material_buffer, material_alloc);
    vkDestroyShaderEXT_(dev, shaders[0], NULL);
    vkDestroyShaderEXT_(dev, shaders[1], NULL);

    for (int i = 0; i < MAX_FRAMES; i++) {
        vkDestroySemaphore(dev, sem_available[i], NULL);
        vkDestroyFence(dev, fences[i], NULL);
    }

    for (uint32_t i = 0; i < window->image_count; i++) {
        vkDestroySemaphore(dev, sem_finished[i], NULL);
    }

    free(sem_finished);
    vkDestroyCommandPool(dev, cmd_pool, NULL);
    destroy_window(window);
    destroy_device();
    glfwTerminate();

    return 0;
}

// abstraction

// gleam usage:

// renderer(frag, vert)
// draw(renderer, mesh)

// renderer(frag, vert)
