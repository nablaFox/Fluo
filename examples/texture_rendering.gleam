import fluo/mesh.{Vec2, Vec3, Vertex}
import fluo/render.{type Renderer}
import fluo/texture.{type Texture}
import fluo/window.{draw}
import gleam/bit_array
import gleam/int
import gleam/list

pub fn main() {
  let window = window.create_window("Fluo Window", width: 800, height: 600)

  let triangle = mesh.load_obj("assets/triangle.obj")

  let quad =
    mesh.create_mesh(
      [
        Vertex(Vec3(-1.0, -1.0, 0.0), Vec3(0.0, 0.0, 1.0), Vec2(0.0, 0.0)),
        Vertex(Vec3(-1.0, 1.0, 0.0), Vec3(0.0, 0.0, 1.0), Vec2(0.0, 1.0)),
        Vertex(Vec3(1.0, 1.0, 0.0), Vec3(0.0, 0.0, 1.0), Vec2(1.0, 1.0)),
        Vertex(Vec3(1.0, -1.0, 0.0), Vec3(0.0, 0.0, 1.0), Vec2(1.0, 0.0)),
      ],
      [0, 1, 2, 2, 3, 0],
    )

  let texture = {
    let width = 512
    let height = 512

    int.range(0, width * height - 1, [], list.prepend)
    |> list.map(fn(_) { <<0, 0, 0, 255>> })
    |> bit_array.concat
    |> texture.create_texture(width, height)
  }

  let triangle_renderer: Renderer(Texture, Nil, Nil) =
    render.create_renderer(
      vert: "shader.vert",
      frag: "texture.frag",
      material: texture,
    )

  let texture_renderer: Renderer(Nil, Nil, Float) =
    render.create_renderer(
      vert: "shader.vert",
      frag: "shader.frag",
      material: Nil,
    )

  let render_texture = fn(alpha) {
    let tex_viewport = #(0, 0, texture.color.width, texture.color.height)
    let tex_scissor = #(0, 0, texture.color.width, texture.color.height)

    use cmd <- render.run_immediate()

    use frame <- render.render_color_frame(cmd, texture.color)

    quad
    |> render.create_drawer(frame, texture_renderer, Nil)(
      alpha,
      tex_viewport,
      tex_scissor,
    )
  }

  use ctx, _ <- window.loop(window, Nil)

  case ctx.keys_down {
    [window.KeyA] -> render_texture(0.5)
    [window.KeyS] -> render_texture(1.0)
    _ -> Nil
  }

  triangle |> draw(ctx, triangle_renderer, Nil)
}
