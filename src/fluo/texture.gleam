import fluo/image
import gleam/dynamic.{type Dynamic}

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
