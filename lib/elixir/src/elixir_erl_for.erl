-module(elixir_erl_for).
-export([translate/4]).
-include("elixir.hrl").

translate(Meta, Args, Return, S) ->
  Ann = ?ann(Meta),
  {Cases, [{do, Expr} | Opts]} = elixir_utils:split_last(Args),

  {TInto, SI} =
    case lists:keyfind(into, 1, Opts) of
      {into, Into} -> elixir_erl_pass:translate(Into, S);
      false when Return -> {{nil, Ann}, S};
      false -> {false, S}
    end,

  TUniq = lists.keyfind(uniq, 1, Opts) == {uniq, true},

  {TCases, SC} = translate_gen(Meta)

translate_gen(ForMeta, [{'<-', Meta, [Left, Right]} | T], Acc, S) ->
  {TLeft, TRight, TFilters, TT, TS} = translate_gen(Meta, Left, Right, T, S),
  TAcc = [{enum, }]
