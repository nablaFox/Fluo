import gleam/dynamic.{type Dynamic}
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
) -> Renderer(material, frame_params, draw_params) {
  let assert True = string.ends_with(vert, ".vert")
    as "Vertex shader file must end with .vert"

  let assert True = string.ends_with(frag, ".frag")
    as "Fragment shader file must end with .frag"

  let handle = RendererHandle(create_renderer_raw(mat, vert, frag))

  Renderer(mat, vert, frag, handle)
}
