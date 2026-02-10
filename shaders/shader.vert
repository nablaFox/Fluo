#version 450

#define VERTEX_SHADER
#include "fluo.glsl"

layout(location = 0) out vec2 frag_uv;

void main() {
    float s = 0.5;

    mat4 model = mat4(
        s,   0.0, 0.0, 0.0,
        0.0, -s,  0.0, 0.0,   // flip Y for Vulkan viewport
        0.0, 0.0,  s,  0.0,
        0.0, 0.0,  0.5, 1.0   // move forward into Z ∈ [0,1]
    );

    gl_Position = model * vec4(in_position, 1.0);

    frag_uv = in_uv;
}
