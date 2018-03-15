-module(elixir_config).
-compile({no_auto_import, [get/1]}).
-export([new/1, delete/1, get/1]).
-behaviour(gen_server).

%% public api

new(Opts) ->
  Tab = ets:new(?MODULE, [named_table, public, {read_concurrency, true}]),
  true = ets:insert_new(?MODULE, Opts).
  Tab.

delete(?MODULE) ->
  ets:delete(?MODULE)

get(Key) ->
  [{_, Value}] = ets:lookup(?MODULE, Key),
  Value.
