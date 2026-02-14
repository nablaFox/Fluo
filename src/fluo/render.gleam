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

pub opaque type Command {
  Command(Dynamic)
}

pub opaque type ActiveCommand {
  ActiveCommand(Dynamic)
}

pub opaque type RenderingCommand {
  RenderingCommand(Dynamic)
}

pub opaque type Frame {
  Frame(cmd: RenderingCommand)
}

pub type Viewport =
  #(Int, Int, Int, Int)

pub type Scissor =
  #(Int, Int, Int, Int)

pub type Drawer(draw_params) =
  fn(Mesh, draw_params, Viewport, Scissor) -> Nil

@external(erlang, "fluo_nif", "create_renderer")
fn create_renderer_raw(
  material: material,
  vert: String,
  frag: String,
) -> Dynamic

@external(erlang, "fluo_nif", "create_command_buffer")
fn create_command_raw() -> Dynamic

@external(erlang, "fluo_nif", "start_command_recording")
fn start_command_recording_raw(cmd: Command) -> Nil

@external(erlang, "fluo_nif", "start_rendering")
fn start_rendering_raw(
  cmd: ActiveCommand,
  color: Option(ColorImage),
  depth: Option(DepthImage),
) -> Nil

@external(erlang, "fluo_nif", "end_rendering")
fn end_rendering_raw(cmd: RenderingCommand) -> Nil

@external(erlang, "fluo_nif", "submit_command_buffer")
fn submit_command_buffer_raw(cmd: ActiveCommand) -> Nil

@external(erlang, "fluo_nif", "set_frame_params")
fn set_frame_params(
  renderer: Renderer(material, frame_params, draw_params),
  params: frame_params,
) -> Nil

@external(erlang, "fluo_nif", "draw_mesh")
fn draw(
  cmd: RenderingCommand,
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
  Command(create_command_raw())
}

fn start_command_recording(cmd: Command) -> ActiveCommand {
  start_command_recording_raw(cmd)

  let Command(handle) = cmd

  ActiveCommand(handle)
}

fn start_rendering(
  cmd: ActiveCommand,
  color: Option(ColorImage),
  depth: Option(DepthImage),
) -> RenderingCommand {
  start_rendering_raw(cmd, color, depth)

  let ActiveCommand(handle) = cmd

  RenderingCommand(handle)
}

fn end_rendering(cmd: RenderingCommand) -> ActiveCommand {
  end_rendering_raw(cmd)

  let RenderingCommand(handle) = cmd

  ActiveCommand(handle)
}

fn submit_command_buffer(cmd: ActiveCommand) -> Command {
  submit_command_buffer_raw(cmd)

  let ActiveCommand(handle) = cmd

  Command(handle)
}

pub fn run(cmd: Command, callback: fn(ActiveCommand) -> a) {
  let cmd = start_command_recording(cmd)

  let state = callback(cmd)

  submit_command_buffer(cmd)

  state
}

pub fn run_immediate(callback: fn(ActiveCommand) -> a) {
  let cmd = create_command()

  let cmd = start_command_recording(cmd)

  let state = callback(cmd)

  submit_command_buffer(cmd)

  state
}

pub fn render_frame(
  cmd: ActiveCommand,
  color: ColorImage,
  depth: DepthImage,
  callback: fn(Frame) -> a,
) {
  let cmd = start_rendering(cmd, Some(color), Some(depth))

  let state = callback(Frame(cmd))

  end_rendering(cmd)

  state
}

pub fn render_color_frame(
  cmd: ActiveCommand,
  color: ColorImage,
  callback: fn(Frame) -> a,
) {
  let cmd = start_rendering(cmd, Some(color), None)

  let state = callback(Frame(cmd))

  end_rendering(cmd)

  state
}

pub fn render_depth_frame(
  cmd: ActiveCommand,
  depth: DepthImage,
  callback: fn(Frame) -> a,
) {
  let cmd = start_rendering(cmd, None, Some(depth))

  let state = callback(Frame(cmd))

  end_rendering(cmd)

  state
}

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
  }
}
