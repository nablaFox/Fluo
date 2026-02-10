import gleam/dynamic.{type Dynamic}
import gleam/option.{Some}
import image.{
  type ColorImage, type DepthImage, create_color_image, create_depth_image,
}
import mesh.{type Mesh}
import render.{type Renderer, draw, end_rendering, start_rendering}

pub type Event {
  None
}

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

@external(erlang, "fluo_nif", "create_window")
fn create_window_raw(title: String, width: Int, height: Int) -> Dynamic

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

@external(erlang, "fluo_nif", "swap_buffers")
pub fn swap(window: Window, color: ColorImage) -> Nil

pub fn present(window: Window) -> Nil {
  swap(window, window.color)
}

@external(erlang, "fluo_nif", "window_should_close")
pub fn window_should_close(window: Window) -> Bool

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

pub fn loop(
  window: Window,
  state: state,
  callback: fn(Event, fn(Renderer, Mesh, params) -> Nil, Float, state) -> state,
) {
  case window_should_close(window) {
    True -> Nil
    False -> {
      start_rendering()

      let delta = 0.016

      let draw = fn(renderer, mesh, params) {
        let color = Some(window.color)
        let depth = Some(window.depth)

        draw(renderer, mesh, params, color, depth)
      }

      let new_state = callback(None, draw, delta, state)

      end_rendering()

      present(window)

      loop(window, new_state, callback)
    }
  }
}
