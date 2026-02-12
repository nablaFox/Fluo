import gleam/dynamic.{type Dynamic}

pub type ColorImage {
  ColorImage(width: Int, height: Int, handle: Dynamic)
}

pub type DepthImage {
  DepthImage(width: Int, height: Int, handle: Dynamic)
}

@external(erlang, "fluo_nif", "create_color_image")
fn create_color_image_raw(width: Int, height: Int) -> Dynamic

@external(erlang, "fluo_nif", "create_depth_image")
fn create_depth_image_raw(width: Int, height: Int) -> Dynamic

@external(erlang, "fluo_nif", "read_image")
fn read_image_raw(handle: Dynamic) -> BitArray

@external(erlang, "fluo_nif", "save_color_image")
pub fn save_color_image(image: ColorImage, path: String) -> Nil

pub fn read_color(image: ColorImage) -> BitArray {
  read_image_raw(image.handle)
}

pub fn read_depth(image: DepthImage) -> BitArray {
  read_image_raw(image.handle)
}

pub fn create_color_image(width: Int, height: Int) -> ColorImage {
  let handle = create_color_image_raw(width, height)

  ColorImage(width, height, handle)
}

pub fn create_depth_image(width: Int, height: Int) -> DepthImage {
  let handle = create_depth_image_raw(width, height)

  DepthImage(width, height, handle)
}
