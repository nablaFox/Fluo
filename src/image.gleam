import gleam/dynamic.{type Dynamic}

pub opaque type ColorImage {
  ColorImage(width: Int, height: Int, handle: Dynamic)
}

pub opaque type DepthImage {
  DepthImage(width: Int, height: Int, handle: Dynamic)
}

@external(erlang, "fluo_nif", "create_color_image")
fn create_color_image_raw(width: Int, height: Int) -> Dynamic

@external(erlang, "fluo_nif", "create_depth_image")
fn create_depth_image_raw(width: Int, height: Int) -> Dynamic

pub fn create_color_image(width: Int, height: Int) -> ColorImage {
  let handle = create_color_image_raw(width, height)

  ColorImage(width, height, handle)
}

pub fn create_depth_image(width: Int, height: Int) -> DepthImage {
  let handle = create_depth_image_raw(width, height)

  DepthImage(width, height, handle)
}
