-module(elixir_erl_for).
-export([translate/4]).
-include("elixir.hrl").

translate(Meta, Args, Return, S) ->
  Ann = ?ann(Meta),
  {Cases, [{do, Expr} | Opts]} = elixir_utils:split_last(Args),

  {TInto, SI} =
    case lists:keyfind(into, 1, Opts) of
      {into, Into} -> elixir_erl_pass:translate
