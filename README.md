# Fluo

> Simplicity is the highest sophistication ~ chopin

Simple and elegant vulkan renderer for Gleam ⭐, a functional programming language.
Currently under development.

#### Gleam cpu code

```gleam
import fluo/geometry
import fluo/key
import fluo/renderer.{type Renderer}
import fluo/window.{draw}

pub fn main() {
  let window = window.create_window("Fluo Window", width: 800, height: 600)

  let triangle = geometry.create_triangle()

  let renderer: Renderer(Nil, Float, Nil) =
    renderer.create_renderer(
      vert: "shader.vert",
      frag: "shader.frag",
      material: Nil,
    )

  use ctx, alpha <- window.loop(window, 0.0)

  let alpha = case ctx.keys_down {
    [key.Space] -> alpha +. ctx.delta
    _ -> alpha
  }

  draw(triangle, ctx, renderer, alpha, Nil)

  alpha
}
```

#### Vertex shader (shaders/shader.vert)

```glsl
void main() {
    gl_Position = vec4(in_position, 1.0);
}
```

#### Fragment shader (shaders/shader.frag)

```glsl
DEF_FRAME_PARAMS({
    float alpha;
});

void main() { 
    out_color = vec4(vec3(1.0, 0.0, 0.0) * F_PARAMS.alpha, 1.0);
}
```

Checkout more examples in the [examples](./examples) folder.
