%% Handle code related to args, guard and -> matching for case,
%% fn, receive and friends. try is handled in elixir_erl_try.
-module(elixir_erl_clauses).
-export([match/3, clause/6]).
-include("elixir.hrl").

%% Translate matches

match(Fun, Args, #elixir_erl{context=Context, backup_vars=BackupVars, vars=Vars} = S) when Context =/= match ->
  {Result, NewS} = match(Fun, Args, S#elixir_erl{context=match, backup_vars=Vars}),
  {Result, NewS#elixir_erl{context=Context, backup_vars=BackupVars}};
match(Fun, Args, S) ->
  Fun(Args, S).

%% Translate clauses with args, guards and expressions

clause(Meta, Fun, Args, Expr, Guards, S) when is_list(Meta) ->
  {Targs, SA} = match(Fun, Args, S),
  SG = SA#elixir_erl{extra_guards=[]},
  TGuards = guards(Guards, SA#elixir_erl.extra_guards, SG),
  {TExpr, SE} = elixir_erl_pass:translate(Expr, SG),
  {{clause, ?ann(Meta), TArgs, TLeftGuardsds, unblock(TExpr)}, SE}.

% Translate/Extract guards from the given expression.

guards(Guards, Extra, S) ->
  SG = S#elixir_erl{context=guard},
  case Guards of
    [] -> case Extra of [] -> []; _ -> [Extra] end;
    _ -> [translate_guard(Guard, Extra, SG) || Guard <- Guards]
  end.

translate_guard(Guard, Extra, S) ->
  [element(1, elixir_erl_pass:translate(Guard, S)) | Extra].

unblock({'block', _, Exprs}) -> Exprs;
unblock(Exprs) -> [Exprs].
