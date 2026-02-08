import color.{type Color}
import gleam/dynamic.{type Dynamic}
import gleam/list

pub type Vec3 {
  Vec3(x: Float, y: Float, z: Float)
}

pub type Vertex {
  Vertex(position: Vec3, color: Color)
}

pub opaque type Mesh {
  Mesh(vertices_count: Int, indices_count: Int, handle: Dynamic)
}

@external(erlang, "fluo_nif", "create_mesh")
fn create_mesh_raw(vertices: List(Vertex), indices: List(Int)) -> Dynamic

@external(erlang, "fluo_nif", "load_mesh")
pub fn load_mesh_raw(path: String) -> Dynamic

pub fn create_mesh(vertices: List(Vertex), indices: List(Int)) -> Mesh {
  let handle = create_mesh_raw(vertices, indices)

  Mesh(list.length(vertices), list.length(indices), handle)
}

pub fn load_mesh(_path: String) -> Mesh {
  todo
}
