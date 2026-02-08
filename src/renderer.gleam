import gleam/dynamic.{type Dynamic}
import gleam/option.{type Option}
import image.{type ColorImage, type DepthImage}
import mesh.{type Mesh}

pub type Param {
  Bool(Bool)
  F32(Float)
  Vec2(Float, Float)
  Vec3(Float, Float, Float)
  Vec4(Float, Float, Float, Float)
  Mat3(Float, Float, Float, Float, Float, Float, Float, Float, Float)
  Mat4(
    Float,
    Float,
    Float,
    Float,
    Float,
    Float,
    Float,
    Float,
    Float,
    Float,
    Float,
    Float,
    Float,
    Float,
    Float,
    Float,
  )
}

pub opaque type Renderer {
  Renderer(
    params: List(Param),
    frag_name: String,
    vert_name: String,
    handle: Dynamic,
  )
}

@external(erlang, "fluo_nif", "create_renderer")
pub fn create_renderer_raw(
  params: List(Param),
  vert vert: String,
  frag frag: String,
) -> #(String, String, Dynamic)

pub fn create_renderer(
  params: List(Param),
  vert vert: String,
  frag frag: String,
) -> Renderer {
  let #(vert_name, frag_name, handle) = create_renderer_raw(params, vert, frag)

  Renderer(params, vert_name, frag_name, handle)
}

@external(erlang, "fluo_nif", "start_rendering")
pub fn start_rendering() -> Nil

@external(erlang, "fluo_nif", "draw")
pub fn draw(
  renderer: Renderer,
  mesh: Mesh,
  params: params,
  color color: Option(ColorImage),
  depth depth: Option(DepthImage),
) -> Nil

@external(erlang, "fluo_nif", "end_rendering")
pub fn end_rendering() -> Nil
