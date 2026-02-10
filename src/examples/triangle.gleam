import color
import mesh
import render
import window

pub fn main() {
  let window = window.create_window("Fluo Window", width: 800, height: 600)

  let mesh =
    mesh.create_mesh(
      [
        mesh.Vertex(mesh.Vec3(0.0, -0.5, 0.0), color.red),
        mesh.Vertex(mesh.Vec3(-0.5, 0.5, 0.0), color.green),
        mesh.Vertex(mesh.Vec3(0.5, 0.5, 0.0), color.blue),
      ],
      [0, 1, 2],
    )

  let renderer = render.create_renderer(vert: "vert.spv", frag: "frag.spv")

  window.loop(window, 0.0, fn(ctx, alpha) {
    ctx.draw(renderer, mesh, #(alpha))

    let alpha = case ctx.keys_down {
      [window.LShift, window.KeyA] -> alpha +. 0.001
      _ -> alpha
    }

    alpha
  })
}
