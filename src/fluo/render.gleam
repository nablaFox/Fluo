import fluo/image.{type ColorImage, type DepthImage}
import fluo/mesh.{type Mesh}
import gleam/dynamic.{type Dynamic}
import gleam/option.{type Option, None, Some}
import gleam/string

pub opaque type Renderer(material) {
  Renderer(
    material: material,
    frag_name: String,
    vert_name: String,
    handle: Dynamic,
  )
}

@external(erlang, "fluo_nif", "create_renderer")
fn create_renderer_raw(
  material: material,
  vert: String,
  frag: String,
) -> Dynamic

pub fn create_renderer(
  vert vert: String,
  frag frag: String,
  material mat: material,
) -> Renderer(material) {
  let assert True = string.ends_with(vert, ".vert")
    as "Vertex shader file must end with .vert"
  let assert True = string.ends_with(frag, ".frag")
    as "Fragment shader file must end with .frag"

  let handle = create_renderer_raw(mat, vert, frag)

  Renderer(mat, vert, frag, handle)
}

@external(erlang, "fluo_nif", "start_rendering")
fn start_rendering_raw(
  color: Option(ColorImage),
  depth: Option(DepthImage),
) -> Nil

pub fn start_rendering(color: ColorImage, depth: DepthImage) -> Nil {
  start_rendering_raw(Some(color), Some(depth))
}

pub fn start_color_rendering(color: ColorImage) -> Nil {
  start_rendering_raw(Some(color), None)
}

pub fn start_depth_rendering(depth: DepthImage) -> Nil {
  start_rendering_raw(None, Some(depth))
}

@external(erlang, "fluo_nif", "draw_mesh")
pub fn draw(
  renderer: Renderer(material),
  mesh mesh: Mesh,
  params params: params,
  scissor scissor: #(Int, Int, Int, Int),
  viewport viewport: #(Int, Int, Int, Int),
) -> Nil

@external(erlang, "fluo_nif", "end_rendering")
pub fn end_rendering() -> Nil
