import color.{red}
import examples/utils
import gleam/option.{None, Some}
import image
import render

pub fn main() {
  let mesh = utils.create_triagle()

  let renderer = render.create_renderer(vert: "vert.spv", frag: "frag.spv")

  let color = image.create_color_image(500, 500)

  render.start_rendering()

  render.draw(renderer, mesh, #(red.r, red.g, red.b, 1.0), Some(color), None)

  render.end_rendering()

  echo image.read(color)
}
