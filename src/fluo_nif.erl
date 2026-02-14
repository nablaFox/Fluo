-module(fluo_nif).

-export([
  create_window/3,
  window_should_close/1,
  window_keys_down/1,
  window_mouse_pos/1,
  window_mouse_delta/1,
  window_delta_time/1,
  window_capture_mouse/1,
  window_release_mouse/1,
  create_mesh/2,
  load_mesh_from_obj/1,
  create_renderer/3,
  create_depth_image/2,
  create_color_image/2,
  read_image/1,
  create_texture/3,
  load_texture_from_path/1,
  save_color_image/2,
  swap_buffers/3,
  set_frame_params/3,
  create_command/0,
  start_command_recording/1,
  end_command_recording/1,
  submit_command/1,
  start_rendering/3,
  end_rendering/1,
  draw_mesh/6
]).

-nifs([
  create_window/3,
  window_should_close/1,
  window_keys_down/1,
  window_mouse_pos/1,
  window_mouse_delta/1,
  window_delta_time/1,
  window_capture_mouse/1,
  window_release_mouse/1,
  create_mesh/2,
  load_mesh_from_obj/1,
  create_renderer/3,
  create_depth_image/2,
  create_color_image/2,
  read_image/1,
  create_texture/3,
  load_texture_from_path/1,
  save_color_image/2,
  swap_buffers/3,
  set_frame_params/3,
  create_command/0,
  start_command_recording/1,
  end_command_recording/1,
  submit_command/1,
  start_rendering/3,
  end_rendering/1,
  draw_mesh/6
]).

-on_load(init/0).

init() ->
  PrivDir = priv_dir(),
  case fluo_shaders:compile_shaders(PrivDir) of
    ok ->
      case erlang:load_nif(filename:join(PrivDir, "libfluo_nif"), 0) of
        ok -> ok;
        {error, _}=Err ->
          report(init_failed, Err),
          Err
      end;
    {error, _}=Err ->
      report(init_failed, Err),
      Err
  end.

priv_dir() ->
  case code:priv_dir(fluo_nif) of
    {error, bad_name} -> filename:join([filename:dirname(code:which(?MODULE)), "..", "priv"]);
    Dir -> Dir
  end.

report(Tag, Term) ->
  io:format(standard_error, "[~p] ~p~n", [Tag, Term]),
  ok.

create_window(_, _, _) -> erlang:nif_error(not_loaded).
window_should_close(_) -> erlang:nif_error(not_loaded).
window_keys_down(_) -> erlang:nif_error(not_loaded).
window_mouse_pos(_) -> erlang:nif_error(not_loaded).
window_mouse_delta(_) -> erlang:nif_error(not_loaded).
window_delta_time(_) -> erlang:nif_error(not_loaded).
window_capture_mouse(_) -> erlang:nif_error(not_loaded).
window_release_mouse(_) -> erlang:nif_error(not_loaded).
create_mesh(_, _) -> erlang:nif_error(not_loaded).
load_mesh_from_obj(_) -> erlang:nif_error(not_loaded).
create_renderer(_, _, _) -> erlang:nif_error(not_loaded).
create_depth_image(_, _) -> erlang:nif_error(not_loaded).
create_color_image(_, _) -> erlang:nif_error(not_loaded).
read_image(_) -> erlang:nif_error(not_loaded).
create_texture(_, _, _) -> erlang:nif_error(not_loaded).
load_texture_from_path(_) -> erlang:nif_error(not_loaded).
save_color_image(_, _) -> erlang:nif_error(not_loaded).
swap_buffers(_, _, _) -> erlang:nif_error(not_loaded).
set_frame_params(_, _, _) -> erlang:nif_error(not_loaded).
create_command() -> erlang:nif_error(not_loaded).
start_command_recording(_) -> erlang:nif_error(not_loaded).
end_command_recording(_) -> erlang:nif_error(not_loaded).
submit_command(_) -> erlang:nif_error(not_loaded).
start_rendering(_, _, _) -> erlang:nif_error(not_loaded).
end_rendering(_) -> erlang:nif_error(not_loaded).
draw_mesh(_, _, _, _, _, _) -> erlang:nif_error(not_loaded).
