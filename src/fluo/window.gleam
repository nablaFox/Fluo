import fluo/image.{
  type ColorImage, type DepthImage, create_color_image, create_depth_image,
}
import fluo/mesh.{type Mesh}
import fluo/render.{type Renderer, draw, end_rendering, start_rendering}
import gleam/dynamic.{type Dynamic}
import gleam/option.{Some}

pub opaque type Window {
  Window(
    width: Int,
    height: Int,
    title: String,
    color: ColorImage,
    depth: DepthImage,
    handle: Dynamic,
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

@external(erlang, "fluo_nif", "window_poll_events")
pub fn poll_events(window: Window) -> Nil

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

pub fn create_window(
  title: String,
  width width: Int,
  height height: Int,
) -> Window {
  let handle = create_window_raw(title, width, height)
  let color = create_color_image(width, height)
  let depth = create_depth_image(width, height)

  Window(width, height, title, color, depth, handle)
}

pub fn present(window: Window) -> Nil {
  swap(window, window.color)
}

pub fn width(window: Window) -> Int {
  window.width
}

pub fn height(window: Window) -> Int {
  window.height
}

pub fn title(window: Window) -> String {
  window.title
}

pub fn color(window: Window) -> ColorImage {
  window.color
}

pub fn depth(window: Window) -> DepthImage {
  window.depth
}

pub type Context(params) {
  Context(
    draw: fn(Renderer, Mesh, params) -> Nil,
    delta: Float,
    keys_down: List(Key),
    mouse_pos: Position,
    mouse_delta: Position,
    width: Int,
    height: Int,
    color: ColorImage,
    depth: DepthImage,
    title: String,
  )
}

pub fn loop(
  window: Window,
  state: state,
  callback: fn(Context(params), state) -> state,
) {
  case window_should_close(window) {
    True -> Nil
    False -> {
      start_rendering()

      poll_events(window)

      let delta = delta(window)

      let keys_down = keys_down(window)

      let mouse_pos = mouse_position(window)

      let mouse_delta = mouse_delta(window)

      let draw = fn(renderer, mesh, params) {
        let color = Some(window.color)
        let depth = Some(window.depth)

        draw(renderer, mesh, params, color, depth)
      }

      let ctx =
        Context(
          draw,
          delta,
          keys_down,
          mouse_pos,
          mouse_delta,
          window.width,
          window.height,
          window.color,
          window.depth,
          window.title,
        )

      let state = callback(ctx, state)

      end_rendering()

      present(window)

      loop(window, state, callback)
    }
  }
}
