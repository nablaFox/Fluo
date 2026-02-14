import fluo/image
import fluo/mesh
import fluo/render.{type Renderer}

pub fn main() {
  let triangle = mesh.load_obj("assets/suzanne.obj")

  let renderer: Renderer(Nil, Float, Nil) =
    render.create_renderer(
      vert: "shader.vert",
      frag: "shader.frag",
      material: Nil,
    )

  let color = image.create_color_image(500, 500)

  let viewport = #(0, 0, color.width, color.height)
  let scissor = #(0, 0, color.width, color.height)

  {
    use cmd <- render.run_immediate()

    use frame <- render.render_color_frame(cmd, color)

    triangle
    |> render.create_drawer(frame, renderer, 1.0)(Nil, viewport, scissor)
  }

  image.save_color_image(color, "output.png")
}
