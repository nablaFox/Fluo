import color
import mesh.{Vec3, Vertex, create_mesh}
import render.{create_renderer}
import window.{create_window, loop}

pub fn main() {
  let window = create_window("Fluo Window", width: 800, height: 600)

  let mesh =
    create_mesh(
      [
        Vertex(Vec3(0.0, -0.5, 0.0), color.red),
        Vertex(Vec3(-0.5, 0.5, 0.0), color.green),
        Vertex(Vec3(0.5, 0.5, 0.0), color.blue),
      ],
      [0, 1, 2],
    )

  let alpha = 0.0

  let renderer =
    create_renderer([render.F32(alpha)], vert: "vert.spv", frag: "frag.spv")

  loop(window, alpha, fn(_, draw, _, alpha) {
    let alpha = alpha +. 0.0001

    draw(renderer, mesh, [render.F32(alpha)])

    alpha
  })
}
