-module(fluo_nif).

-export([
  create_window/3
]).

-nifs([
  create_window/3
]).

-on_load(init/0).

init() -> erlang:load_nif("priv/libfluo_nif", 0).

create_window(_, _, _) -> erlang:nif_error(not_loaded).
