import fluo/mesh
import fluo/render.{type Renderer}
import fluo/texture.{type Texture}
import fluo/window.{draw}
import gleam/bit_array
import gleam/int
import gleam/list

pub fn main() {
  let window = window.create_window("Fluo Window", width: 800, height: 600)

  let triangle = mesh.load_obj("assets/triangle.obj")

  let texture = {
    let width = 512
    let height = 512
    let tile = 64

    int.range(0, width * height - 1, [], list.prepend)
    |> list.map(fn(i) {
      let x = i % width

      let assert Ok(y) = int.divide(i, width)
      let assert Ok(cx) = int.divide(x, tile)
      let assert Ok(cy) = int.divide(y, tile)

      let #(r, g, b, a) = case { cx + cy } % 2 == 0 {
        True -> #(255, 105, 180, 255)
        False -> #(255, 0, 0, 255)
      }

      <<r, g, b, a>>
    })
    |> bit_array.concat
    |> texture.create_texture(width, height)
  }

  let renderer: Renderer(Texture, Nil, Nil) =
    render.create_renderer(
      vert: "shader.vert",
      frag: "texture.frag",
      material: texture,
    )

  use ctx, _ <- window.loop(window, Nil)

  triangle |> draw(ctx, renderer, Nil)
}
