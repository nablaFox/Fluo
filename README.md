# Fluo

> Simplicity is the highest sophistication ~ chopin

Simple and elegant vulkan renderer for Gleam ⭐, a purely functional programming language.
Currently under development.

#### Gleam cpu code

```gleam
pub fn main() {
  let window = create_window(800, 600, "Fluo Window")

  let vertices = [
    Vertex(position: Vec3(0.0, 0.5, 0.0), color: red),
    Vertex(position: Vec3(-0.5, -0.5, 0.0), color: green),
    Vertex(position: Vec3(0.5, -0.5, 0.0), color: blue),
  ]

  let indices = [0, 1, 2]

  let mesh = create_mesh(vertices, indices)

  let alpha = 0.0

  let renderer = create_renderer(#(alpha), vert: "vert.spv", frag: "frag.spv")

  game_loop(mesh, renderer, window, alpha)
}

fn game_loop(mesh, renderer, window, alpha) {
  case window_should_close(window) {
    True -> Nil
    False -> {
      start_rendering()

      let color = Some(window.color)
      let depth = Some(window.depth)

      let alpha = float.min(alpha +. 0.01, 1.0)

      draw(renderer, mesh, #(alpha), color, depth)

      end_rendering()

      present_window(window)

      game_loop(mesh, renderer, window, alpha)
    }
  }
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
