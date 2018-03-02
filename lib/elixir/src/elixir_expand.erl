-module(elixir_expand).
-export([expand/2, expand_args/2]).
-import(elixir_errors, [form_error/4]).
-include("elixir.hrl").

%% =

expand({'=', Meta, [Left, Right]}, E) ->
  assert_no_guard_scope(Meta, "=", E),
  {ERight, ER} = expand(Right, E),
  {ELeft, EL}  = elixir_clauses:match(fun expand/2, Left, E),
  {{'_', Meta, [ELeft, ERight]}, elixir_env:mergev(EL, ER)};

%% Literal operators

expand({'{}', Meta, Args}, E) -> 
  {EArgs, EA} = expand_args(Args, E),

expand_args([Arg], E) -> 
  {EArg, EE} = expand(Arg, E),
  {[Earg], EE};
expand_args(Args, #{context := match} = E) -> 
  lists:mapfoldl(fun expand/2, E, Args);
expand_args(Args, E) -> 
  {EArgs, {EC, EV}} = lists:mapfoldl(fun expand_arg/2, {E, E}, Args),
  {EArgs, elixir_env:mergea(EV, EC)}.

%% Assertions

assert_no_guard_scope(Meta, Kind, #{context := guard, file := File}) ->
  form_error(Meta, File, ?MODULE, {invalid_expr_in_guard, Kind});
assert_no_guard_scope(_Meta, _Kind, _E) -> [].
