import fluo/image.{type ColorImage, type DepthImage}
import fluo/mesh.{type Mesh}
import gleam/dynamic.{type Dynamic}
import gleam/option.{type Option}

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
  material: material,
  vert vert: String,
  frag frag: String,
) -> Renderer(material) {
  let handle = create_renderer_raw(material, vert, frag)

  Renderer(material, vert, frag, handle)
}

@external(erlang, "fluo_nif", "start_rendering")
pub fn start_rendering() -> Nil

@external(erlang, "fluo_nif", "draw_mesh")
pub fn draw(
  renderer: Renderer(material),
  mesh: Mesh,
  params: params,
  color color: Option(ColorImage),
  depth depth: Option(DepthImage),
) -> Nil

@external(erlang, "fluo_nif", "end_rendering")
pub fn end_rendering() -> Nil
