import color
import mesh.{Vec3, Vertex, create_mesh}
import renderer.{create_renderer}
import window.{create_window, loop}

pub fn main() {
  let window = create_window("Fluo Window", width: 800, height: 600)

  let mesh =
    create_mesh(
      [
        Vertex(Vec3(0.0, 0.5, 0.0), color.red),
        Vertex(Vec3(-0.5, -0.5, 0.0), color.green),
        Vertex(Vec3(0.5, -0.5, 0.0), color.blue),
      ],
      [0, 1, 2],
    )

  let renderer =
    create_renderer([renderer.F32(10.5)], vert: "vert.spv", frag: "frag.spv")

  loop(window, fn(_, draw, delta) {
    // draw(renderer, mesh, #(render.Vec3))

    // let alpha = dict.get(renderer.params, "alpha")

    Nil
  })
}
