import color
import gleam/option.{None, Some}
import image
import mesh.{Vec3, Vertex, create_mesh}
import render

pub fn main() {
  let mesh =
    create_mesh(
      [
        Vertex(Vec3(0.0, -0.5, 0.0), color.red),
        Vertex(Vec3(-0.5, 0.5, 0.0), color.green),
        Vertex(Vec3(0.5, 0.5, 0.0), color.blue),
      ],
      [0, 1, 2],
    )

  let renderer =
    render.create_renderer(
      [render.F32(1.0)],
      vert: "vert.spv",
      frag: "frag.spv",
    )

  let color = image.create_color_image(500, 500)

  render.start_rendering()

  render.draw(renderer, mesh, [render.F32(1.0)], Some(color), None)

  render.end_rendering()

  echo image.read(color)
}
