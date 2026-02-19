import fluo/mesh.{type Mesh, type Vertex, Vec2, Vec3, Vertex}

pub type Geometry {
  Geometry(vertices: List(Vertex), indices: List(Int))
}

pub const triangle = Geometry(
  [
    Vertex(
      position: Vec3(0.0, -0.5, 0.0),
      normal: Vec3(1.0, 0.0, 0.0),
      uv: Vec2(0.5, 0.0),
    ),
    Vertex(
      position: Vec3(-0.5, 0.5, 0.0),
      normal: Vec3(0.0, 1.0, 0.0),
      uv: Vec2(0.0, 1.0),
    ),
    Vertex(
      position: Vec3(0.5, 0.5, 0.0),
      normal: Vec3(0.0, 0.0, 1.0),
      uv: Vec2(1.0, 1.0),
    ),
  ],
  [0, 1, 2],
)

pub const cube = Geometry(
  [
    Vertex(
      position: Vec3(-0.5, -0.5, 0.5),
      normal: Vec3(0.0, 0.0, 1.0),
      uv: Vec2(0.0, 0.0),
    ),
    Vertex(
      position: Vec3(0.5, -0.5, 0.5),
      normal: Vec3(0.0, 0.0, 1.0),
      uv: Vec2(1.0, 0.0),
    ),
    Vertex(
      position: Vec3(0.5, 0.5, 0.5),
      normal: Vec3(0.0, 0.0, 1.0),
      uv: Vec2(1.0, 1.0),
    ),
    Vertex(
      position: Vec3(-0.5, 0.5, 0.5),
      normal: Vec3(0.0, 0.0, 1.0),
      uv: Vec2(0.0, 1.0),
    ),

    Vertex(
      position: Vec3(0.5, -0.5, -0.5),
      normal: Vec3(0.0, 0.0, -1.0),
      uv: Vec2(0.0, 0.0),
    ),
    Vertex(
      position: Vec3(-0.5, -0.5, -0.5),
      normal: Vec3(0.0, 0.0, -1.0),
      uv: Vec2(1.0, 0.0),
    ),
    Vertex(
      position: Vec3(-0.5, 0.5, -0.5),
      normal: Vec3(0.0, 0.0, -1.0),
      uv: Vec2(1.0, 1.0),
    ),
    Vertex(
      position: Vec3(0.5, 0.5, -0.5),
      normal: Vec3(0.0, 0.0, -1.0),
      uv: Vec2(0.0, 1.0),
    ),

    Vertex(
      position: Vec3(-0.5, -0.5, -0.5),
      normal: Vec3(-1.0, 0.0, 0.0),
      uv: Vec2(0.0, 0.0),
    ),
    Vertex(
      position: Vec3(-0.5, -0.5, 0.5),
      normal: Vec3(-1.0, 0.0, 0.0),
      uv: Vec2(1.0, 0.0),
    ),
    Vertex(
      position: Vec3(-0.5, 0.5, 0.5),
      normal: Vec3(-1.0, 0.0, 0.0),
      uv: Vec2(1.0, 1.0),
    ),
    Vertex(
      position: Vec3(-0.5, 0.5, -0.5),
      normal: Vec3(-1.0, 0.0, 0.0),
      uv: Vec2(0.0, 1.0),
    ),

    Vertex(
      position: Vec3(0.5, -0.5, 0.5),
      normal: Vec3(1.0, 0.0, 0.0),
      uv: Vec2(0.0, 0.0),
    ),
    Vertex(
      position: Vec3(0.5, -0.5, -0.5),
      normal: Vec3(1.0, 0.0, 0.0),
      uv: Vec2(1.0, 0.0),
    ),
    Vertex(
      position: Vec3(0.5, 0.5, -0.5),
      normal: Vec3(1.0, 0.0, 0.0),
      uv: Vec2(1.0, 1.0),
    ),
    Vertex(
      position: Vec3(0.5, 0.5, 0.5),
      normal: Vec3(1.0, 0.0, 0.0),
      uv: Vec2(0.0, 1.0),
    ),

    Vertex(
      position: Vec3(-0.5, 0.5, 0.5),
      normal: Vec3(0.0, 1.0, 0.0),
      uv: Vec2(0.0, 0.0),
    ),
    Vertex(
      position: Vec3(0.5, 0.5, 0.5),
      normal: Vec3(0.0, 1.0, 0.0),
      uv: Vec2(1.0, 0.0),
    ),
    Vertex(
      position: Vec3(0.5, 0.5, -0.5),
      normal: Vec3(0.0, 1.0, 0.0),
      uv: Vec2(1.0, 1.0),
    ),
    Vertex(
      position: Vec3(-0.5, 0.5, -0.5),
      normal: Vec3(0.0, 1.0, 0.0),
      uv: Vec2(0.0, 1.0),
    ),

    Vertex(
      position: Vec3(-0.5, -0.5, -0.5),
      normal: Vec3(0.0, -1.0, 0.0),
      uv: Vec2(0.0, 0.0),
    ),
    Vertex(
      position: Vec3(0.5, -0.5, -0.5),
      normal: Vec3(0.0, -1.0, 0.0),
      uv: Vec2(1.0, 0.0),
    ),
    Vertex(
      position: Vec3(0.5, -0.5, 0.5),
      normal: Vec3(0.0, -1.0, 0.0),
      uv: Vec2(1.0, 1.0),
    ),
    Vertex(
      position: Vec3(-0.5, -0.5, 0.5),
      normal: Vec3(0.0, -1.0, 0.0),
      uv: Vec2(0.0, 1.0),
    ),
  ],
  [
    0, 1, 2, 0, 2, 3, 4, 5, 6, 4, 6, 7, 8, 9, 10, 8, 10, 11, 12, 13, 14, 12, 14,
    15, 16, 17, 18, 16, 18, 19, 20, 21, 22, 20, 22, 23,
  ],
)

pub const quad = Geometry(
  [
    Vertex(Vec3(-1.0, -1.0, 0.0), Vec3(0.0, 0.0, 1.0), Vec2(0.0, 0.0)),
    Vertex(Vec3(-1.0, 1.0, 0.0), Vec3(0.0, 0.0, 1.0), Vec2(0.0, 1.0)),
    Vertex(Vec3(1.0, 1.0, 0.0), Vec3(0.0, 0.0, 1.0), Vec2(1.0, 1.0)),
    Vertex(Vec3(1.0, -1.0, 0.0), Vec3(0.0, 0.0, 1.0), Vec2(1.0, 0.0)),
  ],
  [0, 1, 2, 2, 3, 0],
)

pub fn to_mesh(geometry: Geometry) -> Mesh {
  mesh.create(geometry.vertices, geometry.indices)
}

pub fn create_triangle() -> Mesh {
  to_mesh(triangle)
}

pub fn create_cube() -> Mesh {
  to_mesh(cube)
}

pub fn create_quad() -> Mesh {
  to_mesh(quad)
}

pub fn create_sphere(precision: Int) -> Mesh {
  todo
}
