DEF_DRAW_PARAMS({
    float alpha;
});

void main() { 
    out_color = vec4(vec3(1.0, 0.0, 0.0) * PARAMS.alpha, 1.0);
}
