import color.{red}
import gleam/option.{None, Some}
import image
import mesh.{Vec2, Vec3, Vertex}
import render

pub fn main() {
  let mesh =
    mesh.create_mesh(
      [
        Vertex(Vec3(0.0, -0.5, 0.0), Vec3(1.0, 0.0, 0.0), Vec2(0.0, 0.0)),
        Vertex(Vec3(-0.5, 0.5, 0.0), Vec3(0.0, 1.0, 0.0), Vec2(0.0, 1.0)),
        Vertex(Vec3(0.5, 0.5, 0.0), Vec3(0.0, 0.0, 1.0), Vec2(1.0, 1.0)),
      ],
      [0, 1, 2],
    )

  let renderer = render.create_renderer(vert: "vert.spv", frag: "frag.spv")

  let color = image.create_color_image(500, 500)

  render.start_rendering()

  render.draw(renderer, mesh, #(red.r, red.g, red.b, 1.0), Some(color), None)

  render.end_rendering()

  echo image.read(color)
}
