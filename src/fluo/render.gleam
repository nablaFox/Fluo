import fluo/image.{type ColorImage, type DepthImage}
import fluo/mesh.{type Mesh}
import gleam/dynamic.{type Dynamic}
import gleam/option.{type Option, None, Some}
import gleam/string

pub opaque type RendererHandle {
  RendererHandle(Dynamic)
}

pub opaque type CommandHandle {
  CommandHandle(Dynamic)
}

pub opaque type Frame {
  Frame(cmd: CommandHandle)
}

pub type Renderer(material, frame_params, draw_params) {
  Renderer(
    material: material,
    frag_name: String,
    vert_name: String,
    handle: RendererHandle,
  )
}

pub type Command {
  Command(
    create_color_frame: fn(ColorImage) -> Frame,
    create_depth_frame: fn(DepthImage) -> Frame,
    create_frame: fn(ColorImage, DepthImage) -> Frame,
    end_frame: fn(Frame) -> Nil,
    submit: fn() -> Nil,
    handle: CommandHandle,
  )
}

pub type Viewport =
  #(Int, Int, Int, Int)

pub type Scissor =
  #(Int, Int, Int, Int)

pub type Drawer(draw_params) =
  fn(Mesh, draw_params, Viewport, Scissor) -> Frame

@external(erlang, "fluo_nif", "create_renderer")
fn create_renderer_raw(
  material: material,
  vert: String,
  frag: String,
) -> Dynamic

@external(erlang, "fluo_nif", "create_command_buffer")
fn create_command_buffer() -> Dynamic

@external(erlang, "fluo_nif", "start_rendering")
fn start_rendering(
  cmd: CommandHandle,
  color: Option(ColorImage),
  depth: Option(DepthImage),
) -> Nil

@external(erlang, "fluo_nif", "end_rendering")
fn end_rendering(handle: CommandHandle) -> Nil

@external(erlang, "fluo_nif", "submit_command_buffer")
fn submit_command_buffer(handle: CommandHandle) -> Nil

@external(erlang, "fluo_nif", "set_frame_params")
fn set_frame_params(
  renderer: Renderer(material, frame_params, draw_params),
  params: frame_params,
) -> Nil

@external(erlang, "fluo_nif", "draw_mesh")
fn draw(
  command: CommandHandle,
  renderer: Renderer(material, frame_params, draw_params),
  mesh mesh: Mesh,
  params params: params,
  scissor scissor: #(Int, Int, Int, Int),
  viewport viewport: #(Int, Int, Int, Int),
) -> Nil

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

pub fn create_command() -> Command {
  let handle = CommandHandle(create_command_buffer())

  let create_color_frame = fn(color: ColorImage) -> Frame {
    start_rendering(handle, Some(color), None)
    Frame(handle)
  }

  let create_depth_frame = fn(depth: DepthImage) -> Frame {
    start_rendering(handle, None, Some(depth))
    Frame(handle)
  }

  let create_frame = fn(color: ColorImage, depth: DepthImage) -> Frame {
    start_rendering(handle, Some(color), Some(depth))
    Frame(handle)
  }

  let end_frame = fn(_frame: Frame) -> Nil { end_rendering(handle) }

  let submit = fn() -> Nil { submit_command_buffer(handle) }

  Command(
    create_color_frame,
    create_depth_frame,
    create_frame,
    end_frame,
    submit,
    handle,
  )
}

// RULE: should not be called twice per frame
pub fn create_drawer(
  frame: Frame,
  renderer: Renderer(material, frame_params, draw_params),
  frame_params: frame_params,
) -> Drawer(draw_params) {
  set_frame_params(renderer, frame_params)

  fn(
    mesh: Mesh,
    params: draw_params,
    scissor: #(Int, Int, Int, Int),
    viewport: #(Int, Int, Int, Int),
  ) {
    draw(frame.cmd, renderer, mesh, params, scissor, viewport)
    frame
  }
}
