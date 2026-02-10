import gleam/dynamic.{type Dynamic}
import gleam/option.{type Option}
import image.{type ColorImage, type DepthImage}
import mesh.{type Mesh}

pub opaque type Renderer {
  Renderer(frag_name: String, vert_name: String, handle: Dynamic)
}

@external(erlang, "fluo_nif", "create_renderer")
fn create_renderer_raw(vert vert: String, frag frag: String) -> Dynamic

pub fn create_renderer(vert vert: String, frag frag: String) -> Renderer {
  let handle = create_renderer_raw(vert, frag)

  Renderer(vert, frag, handle)
}

@external(erlang, "fluo_nif", "start_rendering")
pub fn start_rendering() -> Nil

@external(erlang, "fluo_nif", "draw_mesh")
pub fn draw(
  renderer: Renderer,
  mesh: Mesh,
  params: params,
  color color: Option(ColorImage),
  depth depth: Option(DepthImage),
) -> Nil

@external(erlang, "fluo_nif", "end_rendering")
pub fn end_rendering() -> Nil
