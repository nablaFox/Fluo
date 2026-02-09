#version 450

layout(location = 0) in vec3 frag_color;
layout(location = 0) out vec4 out_color;

// #include "fluo.glsl"
//
// DEF_MATERIAL({
//     float alpha;
// });

void main() {
    out_color = vec4(frag_color, 1.0);
    // out_color = vec4(frag_color * MATERIAL.alpha, MATERIAL.alpha);
}
