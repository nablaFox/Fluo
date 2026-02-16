import gleam/bit_array
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

pub opaque type MeshAllocator {
  MeshAllocator(Dynamic)
}

pub type Mesh {
  Mesh(
    vertices_count: Int,
    indices_count: Int,
    allocator: MeshAllocator,
    handle: MeshHandle,
  )
}

@external(erlang, "fluo_nif", "create_mesh_allocator")
fn create_mesh_allocator(vertices_count: Int, indices_count: Int) -> Dynamic

@external(erlang, "fluo_nif", "allocate_mesh")
fn allocate_mesh(
  allocator: Dynamic,
  vertices_count: Int,
  indices_count: Int,
) -> Dynamic

@external(erlang, "fluo_nif", "write_mesh")
fn write_mesh(mesh: Dynamic, vertices: BitArray, indices: BitArray) -> Nil

@external(erlang, "fluo_nif", "submit_mesh_writes")
fn write_meshes(allocator: Dynamic) -> Nil

@external(erlang, "fluo_nif", "load_mesh_from_obj")
fn load_obj(path: String) -> #(Int, Int, Dynamic, Dynamic)

fn vertices_count(vertices: BitArray) -> Int {
  bit_array.byte_size(vertices) / { 3 * 4 + 3 * 4 + 2 * 4 }
}

fn indices_count(indices: BitArray) -> Int {
  bit_array.byte_size(indices) / 4
}

fn create_mesh(
  allocator: Dynamic,
  vertices: BitArray,
  indices: BitArray,
) -> Mesh {
  let vertices_count = vertices_count(vertices)
  let indices_count = indices_count(indices)

  let handle = allocate_mesh(allocator, vertices_count, indices_count)

  Mesh(
    vertices_count,
    indices_count,
    MeshAllocator(allocator),
    MeshHandle(handle),
  )
}

pub fn vertices_to_bitarray(vertices: List(Vertex)) -> BitArray {
  todo
}

pub fn indices_to_bitarray(indices: List(Int)) -> BitArray {
  todo
}

pub fn create(vertices: List(Vertex), indices: List(Int)) -> Mesh {
  let vertices = vertices_to_bitarray(vertices)
  let indices = indices_to_bitarray(indices)

  create_from_bits(vertices, indices)
}

pub fn create_from_bits(vertices: BitArray, indices: BitArray) -> Mesh {
  let vertices_count = vertices_count(vertices)
  let indices_count = indices_count(indices)

  let allocator = create_mesh_allocator(vertices_count, indices_count)

  let mesh = create_mesh(allocator, vertices, indices)

  let MeshHandle(handle) = mesh.handle

  write_mesh(handle, vertices, indices)

  write_meshes(allocator)

  mesh
}

pub fn create_many(
  vertices: List(BitArray),
  indices: List(BitArray),
) -> List(Mesh) {
  let vertices_count =
    list.fold(vertices, 0, fn(acc, vertices) { acc + vertices_count(vertices) })

  let indices_count =
    list.fold(indices, 0, fn(acc, indices) { acc + indices_count(indices) })

  let allocator = create_mesh_allocator(vertices_count, indices_count)

  let meshes = {
    use #(vertices, indices) <- list.map(list.zip(vertices, indices))

    create_mesh(allocator, vertices, indices)
  }

  write_meshes(allocator)

  meshes
}

pub fn create_from_obj(path: String) -> Mesh {
  let #(vertices_count, indices_count, handle, allocator) = load_obj(path)

  let handle = MeshHandle(handle)
  let allocator = MeshAllocator(allocator)

  Mesh(vertices_count, indices_count, allocator, handle)
}
