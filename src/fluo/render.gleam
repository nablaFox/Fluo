import fluo/image.{type ColorImage, type DepthImage}
import fluo/mesh.{type Mesh}
import gleam/dynamic.{type Dynamic}
import gleam/option.{type Option, None, Some}
import gleam/string

pub opaque type RendererHandle {
  RendererHandle(Dynamic)
}

pub type Renderer(material, frame_params, draw_params) {
  Renderer(
    material: material,
    frag_name: String,
    vert_name: String,
    handle: RendererHandle,
  )
}

pub opaque type Frame {
  Frame
}

@external(erlang, "fluo_nif", "create_renderer")
fn create_renderer_raw(
  material: material,
  vert: String,
  frag: String,
) -> Dynamic

@external(erlang, "fluo_nif", "start_rendering")
fn start_rendering_raw(
  color: Option(ColorImage),
  depth: Option(DepthImage),
) -> Nil

@external(erlang, "fluo_nif", "end_rendering")
fn end_rendering_raw() -> Nil

@external(erlang, "fluo_nif", "draw_mesh")
fn draw(
  renderer: Renderer(material, frame_params, draw_params),
  mesh mesh: Mesh,
  params params: params,
  scissor scissor: #(Int, Int, Int, Int),
  viewport viewport: #(Int, Int, Int, Int),
) -> Nil

@external(erlang, "fluo_nif", "set_frame_params")
fn set_frame_params(
  renderer: Renderer(material, frame_params, draw_params),
  params: frame_params,
) -> Nil

pub fn create_drawer(
  _frame: Frame,
  renderer: Renderer(material, frame_params, draw_params),
  frame_params: frame_params,
) -> fn(Mesh, draw_params, #(Int, Int, Int, Int), #(Int, Int, Int, Int)) -> Nil {
  set_frame_params(renderer, frame_params)

  fn(
    mesh: Mesh,
    params: draw_params,
    scissor: #(Int, Int, Int, Int),
    viewport: #(Int, Int, Int, Int),
  ) -> Nil {
    draw(renderer, mesh, params, scissor, viewport)
  }
}

pub fn create_renderer(
  vert vert: String,
  frag frag: String,
  material mat: material,
) -> Renderer(material, frame_params, draw_params) {
  let assert True = string.ends_with(vert, ".vert")
    as "Vertex shader file must end with .vert"
  let assert True = string.ends_with(frag, ".frag")
    as "Fragment shader file must end with .frag"

  let handle = create_renderer_raw(mat, vert, frag)

  let handle = RendererHandle(handle)

  Renderer(mat, vert, frag, handle)
}

pub fn start_rendering(color: ColorImage, depth: DepthImage) -> Frame {
  start_rendering_raw(Some(color), Some(depth))
  Frame
}

pub fn start_color_rendering(color: ColorImage) -> Frame {
  start_rendering_raw(Some(color), None)
  Frame
}

pub fn start_depth_rendering(depth: DepthImage) -> Frame {
  start_rendering_raw(None, Some(depth))
  Frame
}

pub fn end_rendering(_frame: Frame) -> Nil {
  end_rendering_raw()
}
