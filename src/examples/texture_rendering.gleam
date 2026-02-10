import color.{red}
import examples/utils
import gleam/bit_array
import gleam/list
import gleam/option.{None, Some}
import render
import texture
import window

pub fn main() {
  let window = window.create_window("Fluo Window", width: 800, height: 600)

  let triangle = utils.create_triagle()

  let quad = utils.create_quad()

  let texture = {
    let width = 512
    let height = 512

    list.range(0, width * height - 1)
    |> list.map(fn(_) { <<0, 0, 0, 255>> })
    |> bit_array.concat
    |> texture.create_texture(width, height)
  }

  let renderer = render.create_renderer(vert: "vert.spv", frag: "texture.spv")

  let texture_renderer =
    render.create_renderer(vert: "vert.spv", frag: "frag.spv")

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

    ctx.draw(renderer, triangle, #(texture))
  })
}
