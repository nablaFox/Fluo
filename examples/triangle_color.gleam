import fluo/color.{red}
import fluo/mesh
import fluo/render
import fluo/window

pub fn main() {
  let window = window.create_window("Fluo Window", width: 800, height: 600)

  let triangle = mesh.load_obj("assets/triangle.obj")

  let renderer = render.create_renderer(vert: "vert.spv", frag: "frag.spv")

  use ctx, alpha <- window.loop(window, 0.0)

  ctx.draw(renderer, triangle, #(red.r, red.g, red.b, alpha))

  let alpha = case ctx.keys_down {
    [window.Space] -> alpha +. ctx.delta
    _ -> alpha
  }

  alpha
}
