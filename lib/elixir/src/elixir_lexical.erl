%% Module responsible for tracking lexical information.
-module(elixir_lexical).
-export([
  record_remote/3
]).
-include("elixir.hrl").

-define(tracker, 'Elixir.Kernel.LexicalTracker').

record_remote(Module, EnvFunction, Ref) ->
  if_tracker(Ref, fun(Pid) -> ?tracker:remote_reference(Pid, Module, mode(EnvFunction)), ok end).

%% HELPERS

mode(nil) -> compile;
mode({_, _}) -> runtime.

if_tracker(nil, _Callback) -> ok;
if_tracker(Pid, Callback) when is_pid(Pid) -> Callback(Pid).
