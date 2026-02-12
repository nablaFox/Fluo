import fluo/color.{red}
import fluo/mesh
import fluo/render.{create_renderer}
import fluo/window.{create_window}

pub fn main() {
  let window1 = create_window("Fluo Window 1", width: 800, height: 600)
  let window2 = create_window("Fluo Window 2", width: 600, height: 800)

  let triangle = mesh.load_obj("assets/suzanne.obj")

  let renderer = create_renderer(Nil, vert: "shader.vert", frag: "shader.frag")

  game_loop(window1, window2, renderer, triangle, 0.0)
}

fn game_loop(
  window1: window.Window,
  window2: window.Window,
  renderer: render.Renderer(material),
  mesh: mesh.Mesh,
  alpha: Float,
) {
  case window.window_should_close(window1) {
    True -> Nil
    False -> {
      let color = window.color(window1)
      let width = window.width(window1)
      let height = window.height(window1)

      render.start_color_rendering(color)

      render.draw(
        renderer,
        mesh,
        params: #(red.r, red.g, red.b, alpha),
        viewport: #(0, 0, width, height),
        scissor: #(0, 0, width, height),
      )

      render.end_rendering()

      window.present(window1)
      window.swap(window2, color)

      game_loop(window1, window2, renderer, mesh, alpha +. 0.0001)
    }
  }
}
