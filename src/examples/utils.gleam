import mesh.{Vec2, Vec3, Vertex}

pub fn create_triagle() -> mesh.Mesh {
  mesh.create_mesh(
    [
      Vertex(Vec3(0.0, -0.5, 0.0), Vec3(1.0, 0.0, 0.0), Vec2(0.5, 0.0)),
      Vertex(Vec3(-0.5, 0.5, 0.0), Vec3(0.0, 1.0, 0.0), Vec2(0.0, 1.0)),
      Vertex(Vec3(0.5, 0.5, 0.0), Vec3(0.0, 0.0, 1.0), Vec2(1.0, 1.0)),
    ],
    [0, 1, 2],
  )
}

pub fn create_quad() -> mesh.Mesh {
  mesh.create_mesh(
    [
      Vertex(Vec3(-1.0, -1.0, 0.0), Vec3(0.0, 0.0, 1.0), Vec2(0.0, 0.0)),
      Vertex(Vec3(-1.0, 1.0, 0.0), Vec3(0.0, 0.0, 1.0), Vec2(0.0, 1.0)),
      Vertex(Vec3(1.0, 1.0, 0.0), Vec3(0.0, 0.0, 1.0), Vec2(1.0, 1.0)),
      Vertex(Vec3(1.0, -1.0, 0.0), Vec3(0.0, 0.0, 1.0), Vec2(1.0, 0.0)),
    ],
    [0, 1, 2, 2, 3, 0],
  )
}
