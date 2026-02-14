import fluo/image.{type ColorImage, type DepthImage}
import fluo/mesh.{type Mesh}
import fluo/render.{type Frame, type Renderer}
import gleam/dynamic.{type Dynamic}

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

pub type Key {
  KeyA
  KeyB
  KeyC
  KeyD
  KeyE
  KeyF
  KeyG
  KeyH
  KeyI
  KeyJ
  KeyK
  KeyL
  KeyM
  KeyN
  KeyO
  KeyP
  KeyQ
  KeyR
  KeyS
  KeyT
  KeyU
  KeyV
  KeyW
  KeyX
  KeyY
  KeyZ
  ArrowUp
  ArrowDown
  ArrowLeft
  ArrowRight
  Enter
  Escape
  Space
  Backspace
  Tab
  LShift
  RShift
  LCtrl
  RCtrl
  LAlt
  RAlt
}

pub type Position {
  Position(x: Float, y: Float)
}

@external(erlang, "fluo_nif", "create_window")
fn create_window_raw(title: String, width: Int, height: Int) -> Dynamic

@external(erlang, "fluo_nif", "window_should_close")
pub fn window_should_close(window: Window) -> Bool

@external(erlang, "fluo_nif", "window_keys_down")
pub fn keys_down(window: Window) -> List(Key)

@external(erlang, "fluo_nif", "window_mouse_pos")
pub fn mouse_position(window: Window) -> Position

@external(erlang, "fluo_nif", "window_mouse_delta")
pub fn mouse_delta(window: Window) -> Position

@external(erlang, "fluo_nif", "swap_buffers")
pub fn swap(window: Window, color: ColorImage) -> Nil

@external(erlang, "fluo_nif", "window_delta_time")
pub fn delta(window: Window) -> Float

@external(erlang, "fluo_nif", "window_capture_mouse")
pub fn capture_mouse(window: Window) -> Nil

@external(erlang, "fluo_nif", "window_release_mouse")
pub fn release_mouse(window: Window) -> Nil

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

pub fn present(window: Window) -> Nil {
  swap(window, window.color)
}

pub fn drawer(
  ctx: Context,
  renderer: Renderer(material, frame_params, draw_params),
  frame_params: frame_params,
) -> fn(Mesh, draw_params) -> Context {
  fn(mesh, draw_params) {
    let viewport = #(0, 0, ctx.width, ctx.height)
    let scissor = #(0, 0, ctx.width, ctx.height)

    let frame =
      mesh
      |> render.create_drawer(ctx.frame, renderer, frame_params)(
        draw_params,
        scissor,
        viewport,
      )

    Context(..ctx, frame:)
  }
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
    frame: Frame,
    capture_mouse: fn() -> Nil,
    release_mouse: fn() -> Nil,
  )
}

pub fn loop(window: Window, state: state, callback: fn(Context, state) -> state) {
  case window_should_close(window) {
    True -> Nil
    False -> {
      let cmd = render.create_command()

      let frame = cmd.create_frame(window.color, window.depth)

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
          frame,
          capture_mouse,
          release_mouse,
        )

      let state = callback(ctx, state)

      cmd.end_frame(frame)

      cmd.submit()

      present(window)

      loop(window, state, callback)
    }
  }
}
