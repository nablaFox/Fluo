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

  let renderer = render.create_renderer(Nil, vert: "shader.vert", frag: "shader.frag")

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
layout(location = 0) out vec2 frag_uv;

void main() {
    gl_Position = vec4(in_position, 1.0);
}
```

#### Fragment gpu shader

```glsl
layout(location = 0) out vec4 out_color;

DEF_PARAMS({
    vec4 color;
});

void main() {
    out_color = vec4(PARAMS.color.xyz * PARAMS.color.w, 1.0);
}
```

Checkout more examples in the [examples](./examples) folder.
