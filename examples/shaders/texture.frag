layout(location = 0) in vec2 uv;

layout(location = 0) out vec4 out_color;

DEF_MATERIAL({
    uint albedo;
});

void main() {
    out_color = TEXTURE(MATERIAL.albedo, uv);
}
