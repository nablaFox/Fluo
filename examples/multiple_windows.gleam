import fluo/command
import fluo/geometry
import fluo/renderer.{type Renderer}
import fluo/window

pub fn main() {
  let window1 = window.create_window("Fluo Window 1", width: 800, height: 600)
  let window2 = window.create_window("Fluo Window 2", width: 600, height: 800)

  let triangle = geometry.create_triangle()

  let renderer: Renderer(Nil, Float, Nil) =
    renderer.create_renderer(
      material: Nil,
      vert: "shader.vert",
      frag: "shader.frag",
    )

  game_loop(window1, window2, renderer, triangle, 0.0)
}

fn game_loop(
  window1: window.Window,
  window2: window.Window,
  renderer: Renderer(material, Float, Nil),
  mesh: mesh.Mesh,
  alpha: Float,
) {
  let cmd = command.create_command()

  let viewport = #(0, 0, window1.width, window1.height)
  let scissor = #(0, 0, window1.width, window1.height)

  case window.window_should_close(window1) {
    True -> Nil
    False -> {
      {
        use cmd <- command.run(cmd)

        use cmd <- command.render_frame(cmd, window1.color, window1.depth)

        mesh
        |> command.create_drawer(cmd, renderer, alpha)(Nil, viewport, scissor)
      }

      window.present(window1)

      window.swap(window2, window1.color)

      game_loop(window1, window2, renderer, mesh, alpha +. 0.0001)
    }
  }
}
