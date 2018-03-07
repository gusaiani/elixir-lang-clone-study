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
        {EClause, elixir_env:merge_and_check_unused_vars(Acc, EAcc)}
    end
  end,

  {EClauses, EE} = lists:mapfoldl(Transformer, E, Clauses),
  EArities = [fn_arity(Args) || {'->', _, [Args, _]} <- EClauses],

  case lists:usort(EArities) of
    [_] ->
      {{fn, Meta, EClauses}, EE};
    _ ->
      form_error(Meta, ?key(E, file), ?MODULE, clauses_with_different_arities)
  end.

is_invalid_arg({'\\\\', _, _}) -> true;
is_invalid_arg(_) -> false.

fn_arity([{'when', _, Args}]) -> length(Args) - 1;
fn_arity(Args) -> length(Args).

%% Capture

capture(Meta, {'/', _, [{{'.', _, [_, F]} = Dot, RequireMeta, []}, A]}, E) when is_atom(F), is_integer(A) ->
  Args = args_from_arity(Meta, A, E),
  capture_require(Meta, {Dot, RequireMeta, Args}, E, true);

capture(Meta, {'/', _, [{F, _, C}, A]}, E) when is_atom(F), is_integer(A), is_atom(C) ->
  Args = args_from_arity(Meta, A, E),
  ImportMeta =
    case lists:keyfind(import_fa, 1, Meta) of
      {import_fa, {Receiver, Context}} ->
        lists:keystore(context, 1,
          lists:keystore(import, 1, Meta, {import, Receiver}),
          {context, Context}
        );
      false -> Meta
    end,
  capture_import(Meta, {F, ImportMeta, Args}, E, true);

capture(Meta, {{'.', _, [_, Fun]}, _, Args} = Expr, E) when is_atom(Fun), is_list(Args) ->
  capture_require(Meta, Expr, E, is_sequential_and_not_empty(Args));

capture(Meta, {{'.', _, [_]}, _, Args} = Expr, E) when is_list(Args) ->
  capture_expr(Meta, Expr, E, false);

capture(Meta, {'__block__', _, [Expr]}, E) ->
  capture(Meta, Expr, E);

capture(Meta, {'__block__', _, _} = Expr, E) ->
  form_error(Meta, ?key(E, file), ?MODULE, {block_expr_in_capture, Expr});

capture(Meta, {Atom, _, Args} = Expr, E) when is_atom(Atom), is_list(Args) ->
  capture_import(Meta, Expr, E, is_sequential_and_not_empty(Args));

capture(Meta, {Left, Right}, E) ->
  capture(Meta, {'{}', Meta, [Left, Right]}, E);

capture(Meta, List, E) when is_list(List) ->
  capture_expr(Meta, List, E, is_sequential_and_not_empty(List));

capture(Meta, Integer, E) when is_integer(Integer) ->
  form_error(Meta, ?key(E, file), ?MODULE, {capture_arg_outside_of_capture, Integer});

capture(Meta, Arg, E) ->
  invalid_capture(Meta, Arg, E).

capture_import(Meta, {Atom, ImportMeta, Args} = Expr, E, Sequential) ->
  Res = Sequential andalso
        elixir_dispatch:import_function(ImportMeta, Atom, length(Args), E),
  handle_capture(Res, Meta, Expr, E, Sequential).

capture_require(Meta, {{'.', DotMeta, [Left, Right]}, RequireMeta, Args}, E, Sequential) ->
  case escape(Left, E, []) of
    {EscLeft, []} ->
      {ELeft, EE} = elixir_expand:expand(EscLeft, E),
      Res

escape({'&' _, [Pos]}, _E, Dict) when is_integer(Pos), Pos > 0 ->
  Var = {list_to_atom([$x | integer_to_list(Pos)]), [], ?var_context},
  {Var, orddict:store(Pos, Var, Dict)};
escape({'&', Meta, [Pos]}, E, _Dict) when is_integer(Pos) ->
  form_error(Meta, &key(E, file), ?MODULE, {unallowed_capture_arg, Pos});
escape({'&', Meta, _} = Arg, E, _Dict) ->
  form_error(Meta, ?key(E, file), ?MODULE, {nested_capture, Arg});
escape({Left, Meta, Right}, E, Dict0) ->
  {TLeft, Dict1}  = escape(Left, E, Dict0),
  {TRight, Dict2} = escape(Right, E, Dict1),
  {{TLeft, Meta, TRight}, Dict2};
escape({Left, Right}, E, Dict0) ->
  {TLeft, Dict1}  = escape(Left, E, Dict0),
  {TRight, Dict2} = escape(Right, E, Dict1),
  {{TLeft, Tright}, Dict2};
escape(List, E, Dict) when is_list(List) ->
  lists:mapfoldl(fun(X, Acc) -> escape(X, E, Acc) end, Dict, List);
escape(Other, _E, Dict) ->
  {Other, Dict}.

args_from_arity(_Meta, A, _E) when is_integer(A), A >= 0, A =< 255 ->
  [{'&', [], [X]} || X <- lists:seq(1, A)];
args_from_arity(Meta, A, E) ->
  form_error(Meta, ?key(E, file), ?MODULE, {invalid_arity_for_capture, A}).

is_sequential_and_not_empty([])   -> false;
is_sequential_and_not_empty(List) -> is_sequential(List, 1).
