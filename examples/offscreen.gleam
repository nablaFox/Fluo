import fluo/color.{red}
import fluo/image
import fluo/mesh
import fluo/render

pub fn main() {
  let triangle = mesh.load_obj("assets/suzanne.obj")

  let renderer =
    render.create_renderer(
      vert: "shader.vert",
      frag: "shader.frag",
      material: Nil,
    )

  let color = image.create_color_image(500, 500)

  render.start_color_rendering(color)

  render.draw(
    renderer,
    triangle,
    params: #(red.r, red.g, red.b, 1.0),
    scissor: #(0, 0, color.width, color.height),
    viewport: #(0, 0, color.width, color.height),
  )

  render.end_rendering()

  image.save_color_image(color, "output.png")
}
