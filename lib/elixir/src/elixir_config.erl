-module(elixir_config).
-compile({no_auto_import, [get/1]}).
-export([get/1]).
-behaviour(gen_server).

%% public api

get(Key) -> 
  [{_, Value}] = ets:lookup(?MODULE, Key),
  Value.
