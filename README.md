# Fluo

> Simplicity is the highest sophistication ~ chopin

Simple and elegant vulkan renderer for Gleam ⭐, a purely functional programming language.
Currently under development.

#### Gleam cpu code

```gleam
import fluo/color.{red}
import fluo/render
import fluo/mesh
import fluo/window

pub fn main() {
  let window = window.create_window("Fluo Window", width: 800, height: 600)

  let triangle = mesh.load_mesh("triangle.obj")

  let renderer = render.create_renderer(vert: "vert.spv", frag: "frag.spv")

  use ctx, alpha <- window.loop(window, 0.0)

  ctx.draw(renderer, triangle, #(red.r, red.g, red.b, alpha))

  let alpha = case ctx.keys_down {
    [window.Space] -> alpha +. ctx.delta
    _ -> alpha
  }

  alpha
}
```

#### Vertex gpu shader

```glsl
#version 450

#define VERTEX_SHADER
#include "fluo.glsl"

layout(location = 0) out vec2 frag_uv;

void main() {
    gl_Position = model * vec4(in_position, 1.0);

    frag_uv = in_uv;
}
```

#### Fragment gpu shader

```glsl
#version 450

#include "fluo.glsl"

layout(location = 0) out vec4 out_color;

DEF_MATERIAL({
    vec4 color;
});

void main() {
    out_color = vec4(MATERIAL.color.xyz * MATERIAL.color.w, 1.0);
}
```

Checkout more examples in the [examples](./examples) folder.
