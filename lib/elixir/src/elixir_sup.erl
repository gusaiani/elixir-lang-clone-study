-module(elixir_sup).
-behaviour(supervisor).
-export([start_link/0]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, ok).
