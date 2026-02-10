import color.{red}
import examples/triangle
import render
import window

pub fn main() {
  let window = window.create_window("Fluo Window", width: 800, height: 600)

  let triangle = triangle.create_triagle()

  let renderer = render.create_renderer(vert: "vert.spv", frag: "frag.spv")

  window.loop(window, 0.0, fn(ctx, alpha) {
    ctx.draw(renderer, triangle, #(red.r, red.g, red.b, alpha))

    let alpha = case ctx.keys_down {
      [window.LShift, window.KeyA] -> alpha +. 0.001
      _ -> alpha
    }

    alpha
  })
}
