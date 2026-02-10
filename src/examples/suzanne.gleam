import mesh
import render
import texture
import window

pub fn main() {
  let window = window.create_window("Fluo Window", width: 800, height: 600)

  let suzanne = mesh.load_obj("assets/suzanne.obj")

  let texture = texture.load_texture("assets/brick.jpeg")

  let renderer = render.create_renderer(vert: "vert.spv", frag: "texture.spv")

  window.loop(window, Nil, fn(ctx, _) {
    ctx.draw(renderer, suzanne, #(texture))
  })
}
