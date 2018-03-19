%% Compiler backend to Erlang:
-module(elixir_erl).
-export([
         get_ann/1
]).

%% Builds Erlang AST annotation.

get_ann(Opts) when is_list(Opts) ->
  get_ann(Opts, false, 0).

get_ann([{generated, true} | T], _, Line) -> get_ann(T, true, Line);
get_ann([{line, Line} | T], Gen, _) when is_integer(Line) -> get_ann(T, Gen, Line);
get_ann([_ | T], Gen, Line) -> get_ann(T, Gen, Line);
get_ann([], Gen, Line) -> erl_anno:set_generated(Gen, Line).

