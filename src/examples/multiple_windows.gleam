import color
import gleam/option.{None, Some}
import mesh.{Vec3, Vertex, create_mesh}
import render.{create_renderer}
import window.{create_window}

pub fn main() {
  let window1 = create_window("Fluo Window 1", width: 800, height: 600)
  let window2 = create_window("Fluo Window 2", width: 600, height: 800)

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

  game_loop(window1, window2, renderer, mesh, alpha)
}

fn game_loop(
  window1: window.Window,
  window2: window.Window,
  renderer: render.Renderer,
  mesh: mesh.Mesh,
  alpha,
) {
  case window.window_should_close(window1) {
    True -> Nil
    False -> {
      render.start_rendering()

      let color = window.color(window1)

      render.draw(renderer, mesh, [render.F32(alpha)], Some(color), None)

      render.end_rendering()

      window.present(window1)
      window.swap(window2, color)

      game_loop(window1, window2, renderer, mesh, alpha +. 0.0001)
    }
  }
}
