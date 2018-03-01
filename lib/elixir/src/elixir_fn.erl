-module(elixir_fn).
-export([capture/3, expand/3, format_error/1]).
-import(elixir_errors, [form_error/4]).
-include("elixir.hrl").

%% Anonymous functions

expand(Meta, Clauses, E) when is_list(Clauses) ->
  Transformer = fun({_, _, [Left, Right]} = Clause, Acc) ->
    case lists:any(fun is_invalid_arg/1, Left) of
      true ->
        form_error(Meta, ?key(E, file), ?MODULE, defaults_in_args);

      false -> 
        {EClause, EAcc} = elixir_clauses:clause(Meta, fn, fun elixir_clauses:head/2, Clause, Acc),

is_invalid_arg({'\\\\', _, _}) -> true;
is_invalid_arg(_) -> false.
