-module(fluo_nif).

-export([
  create_window/3,
  window_should_close/1,
  create_mesh/2,
  create_renderer/2,
  start_rendering/0,
  draw_mesh/5,
  end_rendering/0,
  swap_buffers/2,
  create_color_image/2,
  create_depth_image/2,
  read_image/1
]).

-nifs([
  create_window/3,
  window_should_close/1,
  create_mesh/2,
  create_renderer/2,
  start_rendering/0,
  draw_mesh/5,
  end_rendering/0,
  swap_buffers/2,
  create_color_image/2,
  create_depth_image/2,
  read_image/1
]).

-on_load(init/0).

init() -> erlang:load_nif("priv/libfluo_nif", 0).

create_window(_, _, _) -> erlang:nif_error(not_loaded).
window_should_close(_) -> erlang:nif_error(not_loaded).
create_mesh(_, _) -> erlang:nif_error(not_loaded).
create_renderer(_, _) -> erlang:nif_error(not_loaded).
start_rendering() -> erlang:nif_error(not_loaded).
draw_mesh(_, _, _, _, _) -> erlang:nif_error(not_loaded).
end_rendering() -> erlang:nif_error(not_loaded).
swap_buffers(_, _) -> erlang:nif_error(not_loaded).
create_color_image(_, _) -> erlang:nif_error(not_loaded).
create_depth_image(_, _) -> erlang:nif_error(not_loaded).
read_image(_) -> erlang:nif_error(not_loaded).
