%% Convenience functions used to manipulate scope and its variables.
-module(elixir_erl_var).
-export([build/2,
         mergec/2
]).
-include("elixir.hrl").

build(Key, #elixir_erl{counter=Counter} = S) ->
  Cnt =
    case maps:find(Key, Counter) of
      {ok, Val} -> Val + 1;
      error -> 1
    end,
  {list_to_atom([$_ | atom_to_list(Key)] ++ "@" ++ integer_to_list(Cnt)),
  Cnt,
  S#elixir_erl{counter=maps:put(Key, Cnt, Counter)}}.

%% Receives two scopes and return the first scope with
%% counters and flags from the later.

mergec(S1, S2) ->
  S1#elixir_erl{
    counter=S2#elixir_erl.counter,
    caller=S2#elixir_erl.caller,
    stacktrace=S2#elixir_erl.stacktrace
  }.
