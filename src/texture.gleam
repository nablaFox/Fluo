import gleam/dynamic.{type Dynamic}
import image

pub opaque type Texture {
  Texture(color: image.ColorImage, handle: Dynamic)
}

@external(erlang, "fluo_nif", "create_texture")
fn create_texture_raw(
  pixels: BitArray,
  width: Int,
  height: Int,
) -> #(image.ColorImage, Dynamic)

pub fn create_texture(pixels: BitArray, width: Int, height: Int) -> Texture {
  let #(color, handle) = create_texture_raw(pixels, width, height)

  Texture(color, handle)
}

pub fn load_texture(path: String) -> Texture {
  todo
}

pub fn color(texture: Texture) -> image.ColorImage {
  texture.color
}
