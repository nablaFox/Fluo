pub type Color {
  Color(r: Float, g: Float, b: Float)
}

pub const red = Color(1.0, 0.0, 0.0)

pub const green = Color(0.0, 1.0, 0.0)

pub const blue = Color(0.0, 0.0, 1.0)

pub const white = Color(1.0, 1.0, 1.0)

pub const black = Color(0.0, 0.0, 0.0)

pub const gray = Color(0.502, 0.502, 0.502)

pub const yellow = Color(1.0, 1.0, 0.0)

pub fn multiply(color: Color, scalar: Float) -> Color {
  let Color(r, g, b) = color
  Color(r *. scalar, g *. scalar, b *. scalar)
}
