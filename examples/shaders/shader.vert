#version 450

#define VERTEX_SHADER
#include "fluo.glsl"

layout(location = 0) out vec2 frag_uv;

void main() {
    gl_Position = vec4(in_position, 1.0);

    frag_uv = in_uv;
}
