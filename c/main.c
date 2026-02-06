#include "device.h"
#include "window.h"

int main(int argc, char* argv[]) {
    init_device();

    struct Window* window = create_window("Vulkan Window", 800, 600);

    while (!window_should_close(window)) {
    }

    vkDeviceWaitIdle(g_device.logical_device);

    destroy_window(window);

    destroy_device();

    glfwTerminate();

    return 0;
}
