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

capture_import(Meta, {Atom, ImportMeta, Args} = Expr, E, Sequential) ->
  Res = Sequential andalso
        elixir_dispatch:import_function(ImportMeta, Atom, length(Args), E),
  handle_capture(Res, Meta, Expr, E, Sequential).

args_from_arity(_Meta, A, _E) when is_integer(A), A >= 0, A =< 255 ->
  [{'&', [], [X]} || X <- lists:seq(1, A)];
args_from_arity(Meta, A, E) ->
  form_error(Meta, ?key(E, file), ?MODULE, {invalid_arity_for_capture, A}).

is_sequential_and_not_empty([])   -> false;
is_sequential_and_not_empty(List) -> is_sequential(List, 1).
