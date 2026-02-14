import fluo/image.{type ColorImage, type DepthImage, type ImageHandle}
import fluo/mesh.{type Mesh, type MeshHandle}
import fluo/renderer.{type Renderer, type RendererHandle}
import gleam/dynamic.{type Dynamic}
import gleam/option.{type Option, None, Some}

pub type Viewport =
  #(Int, Int, Int, Int)

pub type Scissor =
  #(Int, Int, Int, Int)

pub type Drawer(draw_params) =
  fn(Mesh, draw_params, Viewport, Scissor) -> Nil

pub opaque type Command {
  Command(Dynamic)
}

pub opaque type CommandRecording {
  CommandRecording(Dynamic)
}

pub opaque type CommandRendering {
  CommandRendering(Dynamic)
}

pub opaque type CommandRecorded {
  CommandRecorded(Dynamic)
}

@external(erlang, "fluo_nif", "create_command")
fn create_command_raw() -> Dynamic

@external(erlang, "fluo_nif", "start_command_recording")
fn start_command_recording_raw(cmd: Dynamic) -> Nil

@external(erlang, "fluo_nif", "end_command_recording")
fn end_command_recording_raw(cmd: Dynamic) -> Nil

@external(erlang, "fluo_nif", "submit_command")
fn submit_command_raw(cmd: Dynamic) -> Nil

@external(erlang, "fluo_nif", "start_rendering")
fn start_rendering_raw(
  cmd: Dynamic,
  color: Option(ImageHandle),
  depth: Option(ImageHandle),
) -> Nil

@external(erlang, "fluo_nif", "end_rendering")
fn end_rendering_raw(cmd: Dynamic) -> Nil

@external(erlang, "fluo_nif", "draw_mesh")
fn draw_raw(
  cmd: Dynamic,
  renderer: RendererHandle,
  mesh: MeshHandle,
  params params: params,
  scissor scissor: #(Int, Int, Int, Int),
  viewport viewport: #(Int, Int, Int, Int),
) -> Nil

@external(erlang, "fluo_nif", "set_frame_params")
fn set_frame_params(
  cmd: Dynamic,
  renderer: RendererHandle,
  params: frame_params,
) -> Nil

pub fn create_command() -> Command {
  Command(create_command_raw())
}

fn start_command_recording(cmd: Command) -> CommandRecording {
  let Command(handle) = cmd

  start_command_recording_raw(handle)

  CommandRecording(handle)
}

fn start_rendering(
  cmd: CommandRecording,
  color: Option(ColorImage),
  depth: Option(DepthImage),
) -> CommandRendering {
  let CommandRecording(handle) = cmd

  let color = case color {
    Some(image) -> Some(image.handle)
    None -> None
  }

  let depth = case depth {
    Some(image) -> Some(image.handle)
    None -> None
  }

  start_rendering_raw(handle, color, depth)

  CommandRendering(handle)
}

fn end_rendering(cmd: CommandRendering) -> CommandRecording {
  let CommandRendering(handle) = cmd

  end_rendering_raw(handle)

  CommandRecording(handle)
}

fn end_command_recording(cmd: CommandRecording) -> CommandRecorded {
  let CommandRecording(handle) = cmd

  end_command_recording_raw(handle)

  CommandRecorded(handle)
}

fn submit_command(cmd: CommandRecorded) -> Command {
  let CommandRecorded(handle) = cmd

  submit_command_raw(handle)

  Command(handle)
}

pub fn run(cmd: Command, callback: fn(CommandRecording) -> a) {
  let cmd = start_command_recording(cmd)

  let state = callback(cmd)

  let cmd = end_command_recording(cmd)

  submit_command(cmd)

  state
}

pub fn run_immediate(callback: fn(CommandRecording) -> a) {
  let cmd = create_command()

  let cmd = start_command_recording(cmd)

  let state = callback(cmd)

  let cmd = end_command_recording(cmd)

  submit_command(cmd)

  state
}

pub fn render_frame(
  cmd: CommandRecording,
  color: ColorImage,
  depth: DepthImage,
  callback: fn(CommandRendering) -> a,
) {
  let cmd = start_rendering(cmd, Some(color), Some(depth))

  let state = callback(cmd)

  end_rendering(cmd)

  state
}

pub fn render_color_frame(
  cmd: CommandRecording,
  color: ColorImage,
  callback: fn(CommandRendering) -> a,
) {
  let cmd = start_rendering(cmd, Some(color), None)

  let state = callback(cmd)

  end_rendering(cmd)

  state
}

pub fn render_depth_frame(
  cmd: CommandRecording,
  depth: DepthImage,
  callback: fn(CommandRendering) -> a,
) {
  let cmd = start_rendering(cmd, None, Some(depth))

  let state = callback(cmd)

  end_rendering(cmd)

  state
}

pub fn draw(
  cmd cmd: CommandRendering,
  renderer renderer: Renderer(material, frame_params, draw_params),
  mesh mesh: Mesh,
  params params: params,
  scisor scissor: #(Int, Int, Int, Int),
  viewport viewport: #(Int, Int, Int, Int),
) -> CommandRendering {
  let CommandRendering(handle) = cmd

  draw_raw(handle, renderer.handle, mesh.handle, params, scissor, viewport)

  cmd
}

pub fn create_drawer(
  cmd cmd: CommandRendering,
  renderer renderer: Renderer(material, frame_params, draw_params),
  params frame_params: frame_params,
) -> Drawer(draw_params) {
  let CommandRendering(handle) = cmd

  set_frame_params(handle, renderer.handle, frame_params)

  fn(
    mesh: Mesh,
    params: draw_params,
    scissor: #(Int, Int, Int, Int),
    viewport: #(Int, Int, Int, Int),
  ) {
    draw(cmd, renderer, mesh, params, scissor, viewport)
    Nil
  }
}

pub fn handle(command: Command) -> Dynamic {
  let Command(handle) = command
  handle
}
