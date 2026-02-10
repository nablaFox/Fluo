import examples/triangle
import render
import texture
import window

pub fn main() {
  let window = window.create_window("Fluo Window", width: 800, height: 600)

  let triangle = triangle.create_triagle()

  let texture = texture.load_texture("fluo.png")

  let renderer = render.create_renderer(vert: "vert.spv", frag: "texture.spv")

  window.loop(window, Nil, fn(ctx, _) { ctx.draw(renderer, triangle, texture) })
}
