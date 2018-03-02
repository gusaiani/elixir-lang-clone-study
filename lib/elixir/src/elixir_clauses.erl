%% Handle code related to args, guard and -> matching for case,
%% fn, receive and friends. try is handled in elixir_try.
-module(elixir_clauses).
-export([match/3, clause/5, def/2, head/2,
         'case'/3, 'receive'/3, 'try'/3, 'cond'/3, with/3,
         format_error/1]).
-import(elixir_errors, [form_error/4]).
-include("elixir.hrl").

match(Fun, Expr, #{context := match} = E) ->
  Fun(Expr, E);
match(Fun, Expr, #{context := Context, prematch_vars := Prematch, current_vars := Current} = E) ->
  {EExpr, EE} = Fun(Expr, E#{context := match, prematch_vars := Current}),
  {EExpr, EE#{context := Context, prematch_vars := Prematch}}.

clause(Meta, Kind, Fun, {'->', ClauseMeta, [_, _]} = Clause, E) when is_function(Fun, 3) ->
  clause(Meta, Kind, fun(X, Acc) -> Fun(ClauseMeta, X, Acc) end, Clause, E);
clause(_Meta, _Kind, Fun, {'->', Meta, [Left, Right]}, E) ->
  {ELeft, EL}  = Fun(Left, E),
  {ERight, ER} = elixir_expand:expand(TRight, EL),

