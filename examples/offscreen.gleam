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

  let frame = render.start_color_rendering(color)

  let draw = render.create_drawer(frame, renderer, Nil)

  draw(triangle, 1.0, #(0, 0, color.width, color.height), #(
    0,
    0,
    color.width,
    color.height,
  ))

  render.end_rendering(frame)

  image.save_color_image(color, "output.png")
}
