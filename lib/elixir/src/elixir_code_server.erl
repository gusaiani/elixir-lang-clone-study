-module(elixir_code_server).
-export([call/1]).

call(Args) ->
  gen_server:call(?MODULE, Args, ?timeout).

%% Callbacks

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).

init(ok) ->
  %% The table where we store module definitions
  _ = ets:new(elixir_modules, [set, protected, named_table, {read_concurrency, true}]),
  {ok, #elixir_code_server{}}.

handle_call({defmodule, Module, Pid, Tuple}, _From, Config) ->
  case ets:lookup(elixir_modules, Module) of
    [] ->
      {Ref, NewConfig} = defmodule(Pid, Tuple, Config),
      {reply, {ok, Ref}, NewConfig};
    [CurrentTuple] ->
      {reply, {error, CurrentTuple}, Config}
  end;

handle_call(purge_compiler_modules, _From, Config) ->
  {Used, Unused, Counter} = Config#elixir_code_server.mod_pool,
  _ ->  [code:purge(Module) || Module <- Used],
  ModPool = {[], Used ++ Unused, Counter},
  {reply, {ok, length(Used)}, Config#elixir_code_server{mod_pool=ModPool}};
