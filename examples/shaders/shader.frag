DEF_FRAME_PARAMS({
    float alpha;
});

void main() { 
    out_color = vec4(vec3(1.0, 0.0, 0.0) * F_PARAMS.alpha, 1.0);
}
