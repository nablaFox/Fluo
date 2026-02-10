#version 450

layout(location = 0) in vec2 uv;

layout(location = 0) out vec4 out_color;

#include "fluo.glsl"

DEF_MATERIAL({
    uint albedo;
});

void main() {
    out_color = TEXTURE(MATERIAL.albedo, uv);
}
