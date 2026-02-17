import fluo/command.{type Command, type CommandRendering}
import fluo/image.{type ColorImage, type DepthImage, type ImageHandle}
import fluo/key.{type Key}
import fluo/mesh.{type Mesh}
import fluo/renderer.{type Renderer}
import gleam/dynamic.{type Dynamic}
import gleam/list

pub opaque type WindowHandle {
  WindowHandle(Dynamic)
}

pub type Window {
  Window(
    width: Int,
    height: Int,
    title: String,
    color: ColorImage,
    depth: DepthImage,
    handle: WindowHandle,
  )
}

pub opaque type PrivateContext {
  PrivateContext
}

pub type Context {
  Context(
    delta: Float,
    fps: Float,
    keys_down: List(Key),
    mouse_pos: Position,
    mouse_delta: Position,
    width: Int,
    height: Int,
    color: ColorImage,
    depth: DepthImage,
    title: String,
    cmd: CommandRendering,
    screenshot: fn(String) -> Nil,
    axis: fn(Key, Key) -> Float,
    capture_mouse: fn() -> Nil,
    release_mouse: fn() -> Nil,
    priv_: PrivateContext,
  )
}

pub type Position {
  Position(x: Float, y: Float)
}

@external(erlang, "fluo_nif", "create_window")
fn create_window_raw(title: String, width: Int, height: Int) -> Dynamic

@external(erlang, "fluo_nif", "window_should_close")
fn window_should_close_raw(window: Dynamic) -> Bool

@external(erlang, "fluo_nif", "window_keys_down")
fn keys_down_raw(window: Dynamic) -> List(Key)

@external(erlang, "fluo_nif", "window_mouse_pos")
fn mouse_position_raw(window: Dynamic) -> Position

@external(erlang, "fluo_nif", "window_mouse_delta")
fn mouse_delta_raw(window: Dynamic) -> Position

@external(erlang, "fluo_nif", "swap_buffers")
fn swap_raw(window: Dynamic, color: ImageHandle, waiting: Dynamic) -> Nil

@external(erlang, "fluo_nif", "window_delta_time")
fn delta_raw(window: Dynamic) -> Float

@external(erlang, "fluo_nif", "window_capture_mouse")
fn capture_mouse_raw(window: Dynamic) -> Nil

@external(erlang, "fluo_nif", "window_release_mouse")
fn release_mouse_raw(window: Dynamic) -> Nil

pub fn create_window(
  title: String,
  width width: Int,
  height height: Int,
) -> Window {
  let handle = create_window_raw(title, width, height)
  let color = image.create_color_image(width, height)
  let depth = image.create_depth_image(width, height)

  let handle = WindowHandle(handle)

  Window(width, height, title, color, depth, handle)
}

pub fn window_should_close(window: Window) -> Bool {
  let WindowHandle(handle) = window.handle
  window_should_close_raw(handle)
}

pub fn keys_down(window: Window) -> List(Key) {
  let WindowHandle(handle) = window.handle
  keys_down_raw(handle)
}

pub fn mouse_position(window: Window) -> Position {
  let WindowHandle(handle) = window.handle
  mouse_position_raw(handle)
}

pub fn mouse_delta(window: Window) -> Position {
  let WindowHandle(handle) = window.handle
  mouse_delta_raw(handle)
}

pub fn delta(window: Window) -> Float {
  let WindowHandle(handle) = window.handle
  delta_raw(handle)
}

pub fn capture_mouse(window: Window) -> Nil {
  let WindowHandle(handle) = window.handle
  capture_mouse_raw(handle)
}

pub fn release_mouse(window: Window) -> Nil {
  let WindowHandle(handle) = window.handle
  release_mouse_raw(handle)
}

pub fn swap(window: Window, color: ColorImage, waiting: Command) -> Nil {
  let WindowHandle(handle) = window.handle
  swap_raw(handle, color.handle, command.handle(waiting))
}

pub fn present(window: Window, waiting waiting: Command) -> Nil {
  swap(window, window.color, waiting)
}

pub fn drawer(
  ctx: Context,
  renderer: Renderer(material, frame_params, draw_params),
  frame_params: frame_params,
) -> fn(Mesh, draw_params) -> Nil {
  fn(mesh, draw_params) {
    let viewport = #(0, 0, ctx.width, ctx.height)
    let scissor = #(0, 0, ctx.width, ctx.height)

    mesh
    |> command.create_drawer(ctx.cmd, renderer, frame_params)(
      draw_params,
      scissor,
      viewport,
    )
  }
}

pub fn draw(
  mesh: Mesh,
  ctx: Context,
  renderer: Renderer(material, frame_params, draw_params),
  frame_params: frame_params,
  draw_params: draw_params,
) -> Nil {
  let viewport = #(0, 0, ctx.width, ctx.height)
  let scissor = #(0, 0, ctx.width, ctx.height)

  mesh
  |> command.create_drawer(ctx.cmd, renderer, frame_params)(
    draw_params,
    scissor,
    viewport,
  )
}

pub fn loop(window: Window, state: a, callback: fn(Context, a) -> a) {
  let cmd = command.create_command()
  do_loop(window, cmd, state, callback)
}

fn do_loop(
  window: Window,
  cmd: Command,
  state: a,
  callback: fn(Context, a) -> a,
) {
  case window_should_close(window) {
    True -> Nil
    False -> {
      let delta = delta(window)

      let keys_down = keys_down(window)

      let mouse_pos = mouse_position(window)

      let mouse_delta = mouse_delta(window)

      let capture_mouse = fn() { capture_mouse(window) }

      let release_mouse = fn() { release_mouse(window) }

      let fps = case delta {
        0.0 -> 0.0
        _ -> 1.0 /. delta
      }

      let axis = fn(negative, positive) {
        let is_down = fn(key: key.Key) { keys_down |> list.contains(key) }

        case is_down(negative), is_down(positive) {
          True, False -> -1.0
          False, True -> 1.0
          _, _ -> 0.0
        }
      }

      let screenshot = fn(path: String) {
        image.save_color_image(window.color, path)
      }

      let state = {
        use cmd <- command.run(cmd)

        use cmd <- command.render_frame(cmd, window.color, window.depth)

        let ctx =
          Context(
            delta,
            fps,
            keys_down,
            mouse_pos,
            mouse_delta,
            window.width,
            window.height,
            window.color,
            window.depth,
            window.title,
            cmd,
            screenshot,
            axis,
            capture_mouse,
            release_mouse,
            PrivateContext,
          )

        callback(ctx, state)
      }

      present(window, cmd)

      do_loop(window, cmd, state, callback)
    }
  }
}
