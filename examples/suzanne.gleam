import fluo/mesh
import fluo/render
import fluo/texture
import fluo/window

pub fn main() {
  let window = window.create_window("Fluo Window", width: 800, height: 600)

  let suzanne = mesh.load_obj("assets/suzanne.obj")

  let texture = texture.load_texture("assets/brick.jpeg")

  let renderer =
    render.create_renderer(
      vert: "shader.vert",
      frag: "texture.frag",
      material: #(texture),
    )

  use ctx, _ <- window.loop(window, Nil)

  ctx.draw(renderer, suzanne, Nil)
}
