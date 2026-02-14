import fluo/image.{type ColorImage, type DepthImage}
import fluo/mesh.{type Mesh}
import fluo/renderer.{type Renderer}
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
fn start_command_recording_raw(cmd: Command) -> Nil

@external(erlang, "fluo_nif", "end_command_recording")
fn end_command_recording_raw(cmd: CommandRecording) -> Nil

@external(erlang, "fluo_nif", "submit_command")
fn submit_command_raw(cmd: CommandRecorded) -> Nil

@external(erlang, "fluo_nif", "start_rendering")
fn start_rendering_raw(
  cmd: CommandRecording,
  color: Option(ColorImage),
  depth: Option(DepthImage),
) -> Nil

@external(erlang, "fluo_nif", "end_rendering")
fn end_rendering_raw(cmd: CommandRendering) -> Nil

@external(erlang, "fluo_nif", "draw_mesh")
fn draw_raw(
  cmd: CommandRendering,
  renderer: Renderer(material, frame_params, draw_params),
  mesh mesh: Mesh,
  params params: params,
  scissor scissor: #(Int, Int, Int, Int),
  viewport viewport: #(Int, Int, Int, Int),
) -> Nil

pub fn create_command() -> Command {
  Command(create_command_raw())
}

fn start_command_recording(cmd: Command) -> CommandRecording {
  start_command_recording_raw(cmd)

  let Command(handle) = cmd

  CommandRecording(handle)
}

fn start_rendering(
  cmd: CommandRecording,
  color: Option(ColorImage),
  depth: Option(DepthImage),
) -> CommandRendering {
  start_rendering_raw(cmd, color, depth)

  let CommandRecording(handle) = cmd

  CommandRendering(handle)
}

fn end_rendering(cmd: CommandRendering) -> CommandRecording {
  end_rendering_raw(cmd)

  let CommandRendering(handle) = cmd

  CommandRecording(handle)
}

fn end_command_recording(cmd: CommandRecording) -> CommandRecorded {
  end_command_recording_raw(cmd)

  let CommandRecording(handle) = cmd

  CommandRecorded(handle)
}

fn submit_command(cmd: CommandRecorded) -> Command {
  submit_command_raw(cmd)

  let CommandRecorded(handle) = cmd

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
  cmd: CommandRendering,
  renderer: Renderer(material, frame_params, draw_params),
  mesh: Mesh,
  params: params,
  scissor: #(Int, Int, Int, Int),
  viewport: #(Int, Int, Int, Int),
) -> CommandRendering {
  draw_raw(cmd, renderer, mesh, params, scissor, viewport)
  cmd
}

pub fn create_drawer(
  cmd: CommandRendering,
  renderer: Renderer(material, frame_params, draw_params),
  frame_params: frame_params,
) -> Drawer(draw_params) {
  renderer.set_frame_params(renderer, frame_params)

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
