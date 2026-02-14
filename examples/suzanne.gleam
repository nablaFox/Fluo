import fluo/mesh
import fluo/renderer.{type Renderer}
import fluo/texture.{type Texture}
import fluo/window.{draw}

pub fn main() {
  let window = window.create_window("Fluo Window", width: 800, height: 600)

  let suzanne = mesh.load_obj("assets/suzanne.obj")

  let texture = texture.load_texture("assets/brick.jpeg")

  let renderer: Renderer(Texture, Nil, Nil) =
    renderer.create_renderer(
      vert: "shader.vert",
      frag: "texture.frag",
      material: texture,
    )

  use ctx, _ <- window.loop(window, Nil)

  suzanne |> draw(ctx, renderer, Nil)
}
