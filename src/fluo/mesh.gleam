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

pub type Allocator =
  fn(BitArray, BitArray) -> Mesh

@external(erlang, "fluo_nif", "create_mesh_allocator")
fn create_mesh_allocator(vertices_count: Int, indices_count: Int) -> Dynamic

@external(erlang, "fluo_nif", "allocate_mesh")
fn allocate_mesh(
  allocator: Dynamic,
  vertices_count: Int,
  indices_count: Int,
) -> Dynamic

@external(erlang, "fluo_nif", "write_mesh")
fn write_mesh(mesh: MeshHandle, vertices: BitArray, indices: BitArray) -> Nil

@external(erlang, "fluo_nif", "submit_mesh_writes")
fn write_meshes(allocator: Dynamic) -> Nil

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
  {
    use vertex <- list.flat_map(vertices)
    let Vertex(
      position: Vec3(px, py, pz),
      normal: Vec3(nx, ny, nz),
      uv: Vec2(u, v),
    ) = vertex
    [
      <<px:float-32-native>>,
      <<py:float-32-native>>,
      <<pz:float-32-native>>,
      <<nx:float-32-native>>,
      <<ny:float-32-native>>,
      <<nz:float-32-native>>,
      <<u:float-32-native>>,
      <<v:float-32-native>>,
    ]
  }
  |> bit_array.concat
}

pub fn indices_to_bitarray(indices: List(Int)) -> BitArray {
  {
    use index <- list.map(indices)
    <<index:int-32-native>>
  }
  |> bit_array.concat
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

  write_mesh(mesh.handle, vertices, indices)

  write_meshes(allocator)

  mesh
}

pub fn create_many(
  vertices_count: Int,
  indices_count: Int,
  callback: fn(Allocator) -> a,
) -> a {
  let allocator = create_mesh_allocator(vertices_count, indices_count)

  let allocate = fn(vertices: BitArray, indices: BitArray) -> Mesh {
    let mesh = create_mesh(allocator, vertices, indices)

    write_mesh(mesh.handle, vertices, indices)

    mesh
  }

  let result = callback(allocate)

  write_meshes(allocator)

  result
}
