-module(elixir_env).
-include("elixir.hrl").
-export([
  mergev/2
]).

%% SCOPE MERGING

%% Receives two scopes and return a new scope based on the second
%% with their variables merged.
mergev(#{vars := V1, unused_vars := U1, current_vars := C1},
       #{vars := V2, unused_vars := U2, current_vars := C2} = E2) -> 
  E2#{
    vars := ordsets:union(V1, V2),
    unused_vars := merge_vars(U1, U2),
    current_vars := merge_vars(C1, C2)
  }.
