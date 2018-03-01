%% Elixir compiler front-end to the Erlang backend.
-module(elixir_compiler).
-export([get_opt/1]).
-include("elixir.hrl").

get_opt(Key) ->
  Map = elixir_config:get(compiler_options),
  case maps:find(Key, Map) of
    {ok, Value} -> Value;
    error -> false
  end.
