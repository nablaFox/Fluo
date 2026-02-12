layout(location = 0) out vec4 out_color;

DEF_PARAMS({
    vec4 color;
});

void main() {
    out_color = vec4(MATERIAL.color.xyz * MATERIAL.color.w, 1.0);
}
