-module(elixir_expand).
-export([expand/2, expand_args/2, expand_arg/2, format_error/1]).
-import(elixir_errors, [form_error/4]).
-include("elixir.hrl").

%% =

expand({'=', Meta, [Left, Right]}, E) ->
  assert_no_guard_scope(Meta, "=", E),
  {ERight, ER} = expand(Right, E),
  {ELeft, EL} = elixir_clauses:match(fun expand/2, Left, ER, E),
  refute_parallel_bitstring_match(ELeft, ERight, E, ?key(E, context) == match),
  {{'=', Meta, [ELeft, ERight]}, EL};

%% Literal operators

expand({'{}', Meta, Args}, E) ->
  {EArgs, EA} = expand_args(Args, E),
  {{'{}', Meta, EArgs}, EA};

expand({'%{}', Meta, Args}, E) ->
  elixir_map:expand_map(Meta, Args, E);

expand({'%', Meta, [Left, Right]}, E) ->
  elixir_map:expand_struct(Meta, Left, Right, E);

expand({'<<>>', Meta, Args}, E) ->
  elixir_bitstring:expand(Meta, Args, E, false);

expand({'->', Meta, _Args}, E) ->
  form_error(Meta, E, ?MODULE, unhandled_arrow_op)

expand_args([Arg], E) ->
  {EArg, EE} = expand(Arg, E),
  {[Earg], EE};
expand_args(Args, #{context := match} = E) ->
  lists:mapfoldl(fun expand/2, E, Args);
expand_args(Args, E) ->
  {EArgs, {EC, EV}} = lists:mapfoldl(fun expand_arg/2, {E, E}, Args),
  {EArgs, elixir_env:mergea(EV, EC)}.

%% Assertions

refute_parallel_bitstring_match({'<<>>', _, _}, {'<<>>', Meta, _} = Arg, E, true) ->
  form_error(Meta, E, ?MODULE, {parallel_bitstring_match, Arg});
refute_parallel_bistring_match(Left, {'=', _Meta, [MatchLeft, MatchRight]}, E, Parallel) ->
  refute_parallel_bistring(Left, MatchLeft, E, true),
  refute_parallel_bitstring_match(Left, MatchRight, E, Parallel);

assert_no_guard_scope(Meta, Kind, #{context := guard, file := File}) ->
  form_error(Meta, File, ?MODULE, {invalid_expr_in_guard, Kind});
assert_no_guard_scope(_Meta, _Kind, _E) -> [].
