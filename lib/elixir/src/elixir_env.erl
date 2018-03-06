-module(elixir_env).
-include("elixir.hrl").
-export([
  merge_and_check_unused_var/2,
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

%% UNUSED VARS

merge_and_check_unused_vars(#{unused_vars := Unused} = E, #{unused_vars := ClauseUnused}) ->
  E#{unused_vars := merge_and_check_unused_vars(Unused, ClauseUnused, E)}.

merge_and_check_unused_vars(Unused, ClauseUnused, E) ->
  maps:fold(fun(Key, ClauseValue, Acc) ->
    case ClauseValue of
      %% The variable was used...
      false ->
        case Acc of
          %% So we propagate if it was not yet used
          #{Key := Value} when Value /= false ->
            Acc#{Key := false};

          %% Otherwise we don't know it or it was already used
          _ ->
            Acc
        end;

      %% The variable was not used...
      _ ->
        case Acc of
          %% If we know it, there is nothing to propagate
          #{Key := _} ->
            Acc;

          %% Otherwise we must warn
          _ ->
            {{Name, _} = Pair, _} = Key,

            case not_underscored(Name) of
              true ->
                IsShadowing = maps:is_key(Pair, ?key(E, current_vars)),
                Warn = {unused_var, Name, IsShadowing}
                elixir_errors:form_warn([{line, ClauseValue}], ?key(E, file), ?MODULE, Warn)

              false ->
                ok
            end,

            Acc
        end
    end
  end, Unused, ClauseUnused).
