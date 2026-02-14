import gleam/dynamic.{type Dynamic}
import gleam/list

pub type Vec2 {
  Vec2(x: Float, y: Float)
}

pub type Vec3 {
  Vec3(x: Float, y: Float, z: Float)
}

pub type Vertex {
  Vertex(position: Vec3, normal: Vec3, uv: Vec2)
}

pub opaque type MeshHandle {
  MeshHandle(Dynamic)
}

pub type Mesh {
  Mesh(vertices_count: Int, indices_count: Int, handle: MeshHandle)
}

@external(erlang, "fluo_nif", "create_mesh")
fn create_mesh_raw(vertices: List(Vertex), indices: List(Int)) -> Dynamic

@external(erlang, "fluo_nif", "load_mesh_from_obj")
fn load_obj_raw(path: String) -> #(Int, Int, Dynamic)

pub fn create_mesh(vertices: List(Vertex), indices: List(Int)) -> Mesh {
  let handle = create_mesh_raw(vertices, indices)
  let handle = MeshHandle(handle)

  Mesh(list.length(vertices), list.length(indices), handle)
}

pub fn load_obj(path: String) -> Mesh {
  let #(vertices_count, indices_count, handle) = load_obj_raw(path)

  let handle = MeshHandle(handle)

  Mesh(vertices_count, indices_count, handle)
}

pub fn handle(mesh: Mesh) -> Dynamic {
  let MeshHandle(handle) = mesh.handle
  handle
}
