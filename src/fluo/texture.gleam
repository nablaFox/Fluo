import fluo/color.{type Color}
import fluo/image
import gleam/bit_array
import gleam/dynamic.{type Dynamic}
import gleam/float
import gleam/int
import gleam/list

pub opaque type TextureHandle {
  TextureHandle(Dynamic)
}

pub type Texture {
  Texture(color: image.ColorImage, handle: TextureHandle)
}

@external(erlang, "fluo_nif", "create_texture")
fn create_texture_raw(
  pixels: BitArray,
  width: Int,
  height: Int,
) -> #(Dynamic, Dynamic)

@external(erlang, "fluo_nif", "load_texture_from_path")
fn load_texture_raw(path: String) -> #(Dynamic, Dynamic, Int, Int)

pub fn create_texture(pixels: BitArray, width: Int, height: Int) -> Texture {
  let #(color, handle) = create_texture_raw(pixels, width, height)

  let handle = TextureHandle(handle)
  let color = image.create_from_handle(color, width, height)

  Texture(color, handle)
}

pub fn load(path: String) -> Texture {
  let #(color, handle, width, height) = load_texture_raw(path)

  let handle = TextureHandle(handle)
  let color = image.create_from_handle(color, width, height)

  Texture(color, handle)
}

pub fn create_from_color(color: Color) -> Texture {
  let r = float.round(color.r *. 255.0)
  let g = float.round(color.g *. 255.0)
  let b = float.round(color.b *. 255.0)

  int.range(0, 1, [], list.prepend)
  |> list.map(fn(_) { <<r, g, b, 255>> })
  |> bit_array.concat
  |> create_texture(1, 1)
}
