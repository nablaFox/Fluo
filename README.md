# Fluo

> Simplicity is the highest sophistication ~ chopin

Simple and elegant vulkan renderer for Gleam ⭐, a functional programming language.
Currently under development.

#### Gleam cpu code

```gleam
import fluo/mesh
import fluo/render.{type Renderer}
import fluo/window.{drawer}

pub fn main() {
  let window = window.create_window("Fluo Window", width: 800, height: 600)

  let triangle = mesh.load_obj("triangle.obj")

  // nil material 
  // nil frame parameter
  // float draw parameter
  let renderer: Renderer(_, _, Float) =
    render.create_renderer(
      vert: "shader.vert",
      frag: "shader.frag",
      material: Nil,
    )

  use ctx, alpha <- window.loop(window, 0.0)

  let alpha = case ctx.keys_down {
    [window.Space] -> alpha +. ctx.delta
    _ -> alpha
  }

  triangle |> drawer(ctx, renderer, Nil)(alpha)

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
DEF_DRAW_PARAMS({
    float alpha;
});

void main() { 
    out_color = vec4(vec3(1.0, 0.0, 0.0) * PARAMS.alpha, 1.0);
}
```

Checkout more examples in the [examples](./examples) folder.
