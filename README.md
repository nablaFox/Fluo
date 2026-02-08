# Fluo

> Simplicity is the highest sophistication ~ chopin

Simple and elegant vulkan renderer for Gleam ⭐, a purely functional programming language.
Currently under development.

#### Gleam cpu code

```gleam
import fluo.{create_window, load_mesh, create_renderer, game_loop}

pub fn main() {
  let window = create_window("Fluo Window", width: 800, height: 600)

  let mesh = load_mesh("assets/triangle.obj")

  let alpha = 0.0

  let renderer = create_renderer(#(alpha), vert: "vert.spv", frag: "frag.spv")

  game_loop(window, fn(_, draw, delta) {
    let alpha = float.min(alpha +. delta, 1.0)
        
    draw(renderer, mesh, #(alpha))
  })
}
```

#### Vertex gpu shader

```glsl
#version 450

layout(location = 0) in vec2 in_position;
layout(location = 1) in vec3 in_color;

layout(location = 0) out vec3 frag_color;

void main() {
    gl_Position = vec4(in_position, 0.0, 1.0);
    frag_color = in_color;
}
```

#### Fragment gpu shader

```glsl
#version 450

layout(location = 0) in vec3 frag_color;
layout(location = 0) out vec4 out_color;

#include "fluo.glsl"

DEF_MATERIAL({
    float alpha;
});

void main() {
    out_color = vec4(frag_color * MATERIAL.alpha, MATERIAL.alpha);
}
```
