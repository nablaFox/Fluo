import fluo/color.{red}
import fluo/mesh.{Vec2, Vec3, Vertex}
import fluo/render
import fluo/texture
import fluo/window
import gleam/bit_array
import gleam/list
import gleam/option.{None, Some}

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

    list.range(0, width * height - 1)
    |> list.map(fn(_) { <<0, 0, 0, 255>> })
    |> bit_array.concat
    |> texture.create_texture(width, height)
  }

  let renderer =
    render.create_renderer(
      vert: "shader.vert",
      frag: "texture.frag",
      material: #(texture),
    )

  let texture_renderer =
    render.create_renderer(
      vert: "shader.vert",
      frag: "shader.frag",
      material: Nil,
    )

  window.loop(window, Nil, fn(ctx, _) {
    case ctx.keys_down {
      [window.Space] -> {
        render.draw(
          texture_renderer,
          quad,
          #(red.r, red.g, red.b, 1.0),
          Some(texture.color(texture)),
          None,
        )
      }
      _ -> Nil
    }

    ctx.draw(renderer, triangle, Nil)
  })
}
