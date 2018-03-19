%% Handle code related to args, guard and -> matching for case,
%% fn, receive and friends. try is handled in elixir_erl_try.
-module(elixir_erl_clauses).
-export([match/3]).
-include("elixir.hrl").

%% Translate matches

match(Fun, Args, #elixir_erl{context=Context, backup_vars=BackupVars, vars=Vars} = S) when Context =/= match ->
  {Result, NewS} = match(Fun, Args, S#elixir_erl{context=match, backup_vars=Vars}),
  {Result, NewS#elixir_erl{context=Context, backup_vars=BackupVars}};
match(Fun, Args, S) ->
  Fun(Args, S).
