-module(fluo_nif).

-export([
  create_window/3,
  window_should_close/1,
  window_poll_events/1,
  window_keys_down/1,
  window_mouse_pos/1,
  window_mouse_delta/1,
  window_delta_time/1,
  window_capture_mouse/1,
  window_release_mouse/1,
  create_mesh/2,
  load_mesh_from_obj/1,
  create_renderer/3,
  start_rendering/0,
  draw_mesh/5,
  end_rendering/0,
  swap_buffers/2,
  create_color_image/2,
  create_depth_image/2,
  read_image/1,
  create_texture/3,
  load_texture_from_path/1
]).

-nifs([
  create_window/3,
  window_should_close/1,
  window_poll_events/1,
  window_keys_down/1,
  window_mouse_pos/1,
  window_mouse_delta/1,
  window_delta_time/1,
  window_capture_mouse/1,
  window_release_mouse/1,
  create_mesh/2,
  load_mesh_from_obj/1,
  create_renderer/3,
  start_rendering/0,
  draw_mesh/5,
  end_rendering/0,
  swap_buffers/2,
  create_color_image/2,
  create_depth_image/2,
  read_image/1,
  create_texture/3,
  load_texture_from_path/1
]).

-on_load(init/0).

init() ->
    PrivDir =
        case code:priv_dir(fluo_nif) of
            {error, bad_name} ->
                filename:join([filename:dirname(code:which(?MODULE)), "..", "priv"]);
            Dir ->
                Dir
        end,
    erlang:load_nif(filename:join(PrivDir, "libfluo_nif"), 0).

create_window(_, _, _) -> erlang:nif_error(not_loaded).
window_should_close(_) -> erlang:nif_error(not_loaded).
window_poll_events(_) -> erlang:nif_error(not_loaded).
window_keys_down(_) -> erlang:nif_error(not_loaded).
window_mouse_pos(_) -> erlang:nif_error(not_loaded).
window_mouse_delta(_) -> erlang:nif_error(not_loaded).
window_delta_time(_) -> erlang:nif_error(not_loaded).
window_capture_mouse(_) -> erlang:nif_error(not_loaded).
window_release_mouse(_) -> erlang:nif_error(not_loaded).
create_mesh(_, _) -> erlang:nif_error(not_loaded).
load_mesh_from_obj(_) -> erlang:nif_error(not_loaded).
create_renderer(_, _, _) -> erlang:nif_error(not_loaded).
start_rendering() -> erlang:nif_error(not_loaded).
draw_mesh(_, _, _, _, _) -> erlang:nif_error(not_loaded).
end_rendering() -> erlang:nif_error(not_loaded).
swap_buffers(_, _) -> erlang:nif_error(not_loaded).
create_color_image(_, _) -> erlang:nif_error(not_loaded).
create_depth_image(_, _) -> erlang:nif_error(not_loaded).
read_image(_) -> erlang:nif_error(not_loaded).
create_texture(_, _, _) -> erlang:nif_error(not_loaded).
load_texture_from_path(_) -> erlang:nif_error(not_loaded).
