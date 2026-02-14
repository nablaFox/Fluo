import fluo/image
import fluo/mesh
import fluo/render.{type Renderer}

pub fn main() {
  let triangle = mesh.load_obj("assets/suzanne.obj")

  let renderer: Renderer(_, _, Float) =
    render.create_renderer(
      vert: "shader.vert",
      frag: "shader.frag",
      material: Nil,
    )

  let color = image.create_color_image(500, 500)

  let cmd = render.create_command()

  let frame = cmd.create_color_frame(color)

  let viewport = #(0, 0, color.width, color.height)
  let scissor = #(0, 0, color.width, color.height)

  triangle
  |> render.create_drawer(frame, renderer, Nil)(1.0, viewport, scissor)

  cmd.end_frame(frame)

  cmd.submit()

  image.save_color_image(color, "output.png")
}
