import gleam/float
import gleam/option.{type Option, Some}

pub type Color {
  Color(r: Float, g: Float, b: Float)
}

pub type Vec3 {
  Vec3(x: Float, y: Float, z: Float)
}

pub type Vertex {
  Vertex(position: Vec3, color: Color)
}

pub type Index =
  Int

pub opaque type Mesh {
  Mesh(vertices: List(Vertex), indices: List(Index))
}

pub opaque type Window {
  Window(
    width: Int,
    height: Int,
    title: String,
    color: ColorImage,
    depth: DepthImage,
  )
}

pub opaque type Renderer(params) {
  Renderer(params: params, frag_name: String, vert_name: String)
}

pub opaque type ColorImage {
  ColorImage(width: Int, height: Int)
}

pub opaque type DepthImage {
  DepthImage(width: Int, height: Int)
}

@external(erlang, "fluo_nif", "create_window")
pub fn create_window(
  title: String,
  width width: Int,
  height height: Int,
) -> Window

@external(erlang, "fluo_nif", "create_renderer")
pub fn create_renderer(
  params: params,
  vert vert: String,
  frag frag: String,
) -> Renderer(params)

@external(erlang, "fluo_nif", "create_mesh")
pub fn create_mesh(vertices: List(Vertex), indices: List(Index)) -> Mesh

@external(erlang, "fluo_nif", "start_rendering")
pub fn start_rendering() -> Nil

@external(erlang, "fluo_nif", "draw")
pub fn draw(
  renderer: Renderer(params),
  mesh: Mesh,
  params: params,
  color color: Option(ColorImage),
  depth depth: Option(DepthImage),
) -> Nil

@external(erlang, "fluo_nif", "end_rendering")
pub fn end_rendering() -> Nil

@external(erlang, "fluo_nif", "present_window")
pub fn present_window(window: Window) -> Nil

@external(erlang, "fluo_nif", "window_should_close")
pub fn window_should_close(window: Window) -> Bool

const red = Color(1.0, 0.0, 0.0)

const green = Color(0.0, 1.0, 0.0)

const blue = Color(0.0, 0.0, 1.0)

pub fn main() {
  let window = create_window("Fluo Window", width: 800, height: 600)

  echo window.width

  let vertices = [
    Vertex(position: Vec3(0.0, 0.5, 0.0), color: red),
    Vertex(position: Vec3(-0.5, -0.5, 0.0), color: green),
    Vertex(position: Vec3(0.5, -0.5, 0.0), color: blue),
  ]

  let indices = [0, 1, 2]

  let mesh = create_mesh(vertices, indices)

  let alpha = 0.0

  let renderer = create_renderer(#(alpha), vert: "vert.spv", frag: "frag.spv")

  game_loop(mesh, renderer, window, alpha)
}

fn game_loop(mesh, renderer, window, alpha) {
  case window_should_close(window) {
    True -> Nil
    False -> {
      start_rendering()

      let color = Some(window.color)
      let depth = Some(window.depth)

      let alpha = float.min(alpha +. 0.01, 1.0)

      draw(renderer, mesh, #(alpha), color, depth)

      end_rendering()

      present_window(window)

      game_loop(mesh, renderer, window, alpha)
    }
  }
}
