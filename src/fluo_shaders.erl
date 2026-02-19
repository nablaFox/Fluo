-module(fluo_shaders).

-export([compile_shaders/2]).

-define(SHADERS_DIR, "shaders").
-define(APPEND_VERTEX, "VERTEX_SHADER_INPUTS;").
-define(APPEND_FRAGMENT, "FRAGMENT_SHADER_OUTPUTS;").
-define(DEFAULT_DRAW_PARAMS, "DEFAULT_DRAW_PARAMS;").

compile_shaders(PrivDir, SrcDir) ->
  case os:find_executable("glslc") of
    false ->
      {error, glslc_not_found};
    Glslc ->
      PrefixPath = filename:join(PrivDir, "fluo.glsl"),
      case file:read_file(PrefixPath) of
        {error, E} ->
          {error, {cannot_read_prefix, PrefixPath, E}};
        {ok, PrefixBin} ->
          case find_shaders(SrcDir) of
            {error, _} = Err -> Err;
            {ok, ShaderPaths} ->
              compile_all(Glslc, PrefixBin, PrivDir, ?SHADERS_DIR, ShaderPaths)
          end
      end
  end.

find_shaders(Dir) ->
  case file:list_dir(Dir) of
    {error, E} ->
      {error, {cannot_list_dir, Dir, E}};
    {ok, Names} ->
      lists:foldl(fun
        (_, {error, _} = Err) ->
          Err;
        (Name, {ok, Acc}) ->
          Path = filename:join(Dir, Name),
          case filelib:is_dir(Path) of
            true ->
              case find_shaders(Path) of
                {error, _} = Err -> Err;
                {ok, Found} -> {ok, Acc ++ Found}
              end;
            false ->
              case is_stage_shader(Name) of
                true  -> {ok, [Path | Acc]};
                false -> {ok, Acc}
              end
          end
      end, {ok, []}, Names)
  end.

compile_all(_Glslc, _Prefix, _PrivDir, _OutDir, []) ->
  ok;
compile_all(Glslc, PrefixBin, PrivDir, OutDir, [ShaderPath | Rest]) ->
  case compile_one(Glslc, PrefixBin, PrivDir, OutDir, ShaderPath) of
    ok -> compile_all(Glslc, PrefixBin, PrivDir, OutDir, Rest);
    {error, _} = Err -> Err
  end.

compile_one(Glslc, PrefixBin, PrivDir, OutDir, ShaderPath) ->
  case stage_of(ShaderPath) of
    none ->
      ok;
    Stage ->
      case file:read_file(ShaderPath) of
        {error, E} ->
          {error, {cannot_read_shader, ShaderPath, E}};
        {ok, ShaderBin} ->
          SourceIolist = combined_source(Stage, PrefixBin, ShaderBin),
          OutName = filename:basename(ShaderPath) ++ ".spv",
          OutPath = filename:join(OutDir, OutName),
          ok = filelib:ensure_dir(OutPath),
          with_temp_glsl(PrivDir, SourceIolist, fun(TmpPath) ->
            run_glslc(Glslc, Stage, TmpPath, OutPath)
          end)
      end
  end.

combined_source(Stage, PrefixBin, ShaderBin) ->
  AppendStage =
    case Stage of
      vert -> <<?APPEND_VERTEX, "\n">>;
      frag -> <<?APPEND_FRAGMENT, "\n">>
    end,
  DrawParams = maybe_default_draw_params(ShaderBin),
  [PrefixBin, <<"\n\n">>, AppendStage, DrawParams, <<"\n">>, ShaderBin].

maybe_default_draw_params(ShaderBin) ->
  case binary:match(ShaderBin, <<"DEF_DRAW_PARAMS">>) of
    nomatch -> <<?DEFAULT_DRAW_PARAMS, "\n">>;
    _ -> <<>>
  end.

with_temp_glsl(PrivDir, Iolist, Fun) ->
  TmpName = io_lib:format(".fluo_tmp_~p.glsl",
                          [erlang:unique_integer([monotonic, positive])]),
  TmpPath = filename:join(PrivDir, lists:flatten(TmpName)),
  case file:write_file(TmpPath, Iolist) of
    ok ->
      try Fun(TmpPath)
      after
        _ = file:delete(TmpPath)
      end;
    {error, E} ->
      {error, {cannot_write_temp, TmpPath, E}}
  end.

run_glslc(Glslc, Stage, InPath, OutPath) ->
  StageArg =
    case Stage of
      vert -> "-fshader-stage=vert";
      frag -> "-fshader-stage=frag"
    end,
  Args = [StageArg, InPath, "-o", OutPath],
  case run_exe(Glslc, Args) of
    {ok, _Output} ->
      ok;
    {error, {exit_status, Code, Output}} ->
      {error, {glslc_failed, InPath, OutPath, Code, Output}};
    {error, Reason} ->
      {error, {glslc_failed, InPath, OutPath, Reason}}
  end.

run_exe(Exe, Args) ->
  Port =
    open_port({spawn_executable, Exe}, [
      {args, Args},
      exit_status,
      use_stdio,
      stderr_to_stdout,
      binary
    ]),
  collect_port(Port, []).

collect_port(Port, Acc) ->
  receive
    {Port, {data, Bin}} ->
      collect_port(Port, [Acc, Bin]);
    {Port, {exit_status, 0}} ->
      {ok, iolist_to_binary(Acc)};
    {Port, {exit_status, Code}} ->
      {error, {exit_status, Code, iolist_to_binary(Acc)}}
  end.

is_stage_shader(Name) ->
  case filename:extension(Name) of
    ".vert" -> true;
    ".frag" -> true;
    _ -> false
  end.

stage_of(Path) ->
  case filename:extension(Path) of
    ".vert" -> vert;
    ".frag" -> frag;
    _ -> none
  end.
