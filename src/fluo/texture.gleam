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
) -> #(image.ColorImage, Dynamic)

@external(erlang, "fluo_nif", "load_texture_from_path")
fn load_texture_raw(path: String) -> #(image.ColorImage, Dynamic)

pub fn create_texture(pixels: BitArray, width: Int, height: Int) -> Texture {
  let #(color, handle) = create_texture_raw(pixels, width, height)

  let handle = TextureHandle(handle)

  Texture(color, handle)
}

pub fn load_texture(path: String) -> Texture {
  let #(color, handle) = load_texture_raw(path)

  let handle = TextureHandle(handle)

  Texture(color, handle)
}
