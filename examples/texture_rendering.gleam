import fluo/mesh.{Vec2, Vec3, Vertex}
import fluo/render
import fluo/texture
import fluo/window.{drawer}
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

  let triangle_renderer: render.Renderer(_, _, Nil) =
    render.create_renderer(
      vert: "shader.vert",
      frag: "texture.frag",
      material: texture,
    )

  let texture_renderer: render.Renderer(_, _, Float) =
    render.create_renderer(
      vert: "shader.vert",
      frag: "shader.frag",
      material: Nil,
    )

  let render_texture = fn(alpha) {
    let tex_cmd = render.create_command()
    let tex_viewport = #(0, 0, texture.color.width, texture.color.height)
    let tex_scissor = #(0, 0, texture.color.width, texture.color.height)

    let tex_frame = tex_cmd.create_color_frame(texture.color)

    quad
    |> render.create_drawer(tex_frame, texture_renderer, Nil)(
      alpha,
      tex_viewport,
      tex_scissor,
    )

    tex_cmd.end_frame(tex_frame)

    tex_cmd.submit()
  }

  use ctx, _ <- window.loop(window, Nil)

  case ctx.keys_down {
    [window.KeyA] -> render_texture(0.5)
    [window.KeyS] -> render_texture(1.0)
    _ -> Nil
  }

  triangle |> drawer(ctx, triangle_renderer, Nil)(Nil)

  Nil
}
