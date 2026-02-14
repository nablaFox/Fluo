import fluo/mesh
import fluo/render.{create_renderer}
import fluo/window.{create_window}

pub fn main() {
  let window1 = create_window("Fluo Window 1", width: 800, height: 600)
  let window2 = create_window("Fluo Window 2", width: 600, height: 800)

  let triangle = mesh.load_obj("assets/suzanne.obj")

  let renderer =
    create_renderer(material: Nil, vert: "shader.vert", frag: "shader.frag")

  game_loop(window1, window2, renderer, triangle, 0.0)
}

fn game_loop(
  window1: window.Window,
  window2: window.Window,
  renderer: render.Renderer(material, Nil, Float),
  mesh: mesh.Mesh,
  alpha: Float,
) {
  case window.window_should_close(window1) {
    True -> Nil
    False -> {
      let color = window1.color

      let cmd = render.create_command()

      let frame = cmd.create_color_frame(color)

      let viewport = #(0, 0, window1.width, window1.height)
      let scissor = #(0, 0, window1.width, window1.height)

      mesh
      |> render.create_drawer(frame, renderer, Nil)(alpha, viewport, scissor)

      cmd.end_frame(frame)

      cmd.submit()

      window.present(window1)
      window.swap(window2, color)

      game_loop(window1, window2, renderer, mesh, alpha +. 0.0001)
    }
  }
}
