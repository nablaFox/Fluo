import fluo/color.{red}
import fluo/image
import fluo/mesh
import fluo/render
import gleam/option.{None, Some}

pub fn main() {
  let triangle = mesh.load_obj("assets/triangle.obj")

  let renderer = render.create_renderer(vert: "vert.spv", frag: "frag.spv")

  let color = image.create_color_image(500, 500)

  render.start_rendering()

  render.draw(
    renderer,
    triangle,
    #(red.r, red.g, red.b, 1.0),
    Some(color),
    None,
  )

  render.end_rendering()

  echo image.read(color)
}
