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

  TUniq = lists:keyfind(uniq, 1, Opts) == {uniq, true},

  {TCases, SC} = translate_gen(Meta, Cases, [], SI),
  {TExpr, SE}  = elixir_erl_pass:translate(wrap_expr(Expr, TInto), SC),
  SF = elixir_erl_var:mergec(SI, SE),

  case comprehension_expr(TInto, TExpr) of
    {inline, TIntoExpr} ->
      build_inline()

translate_gen(ForMeta, [{'<-', Meta, [Left, Right]} | T], Acc, S) ->
  {TLeft, TRight, TFilters, TT, TS} = translate_gen(Meta, Left, Right, T, S),
  TAcc = [{enum, Meta, TLeft, TRight, TFilters} | Acc],
  translate_gen(ForMeta, TT, TAcc, TS);
translate_gen(ForMeta, [{'<<>>', _, [{'<-', Meta, [Left, Right]}]} | T], Acc, S) ->
  {TLeft, TRight, TFilters, TT, TS} = translate_gen(Meta, Left, TRight, T, S),
  TAcc = [{bin, Meta, TLeft, TRight, TFilters} | Acc],
  translate_gen(ForMeta, TT, TAcc, TS);
translate_gen(_ForMeta, [], Acc, S) ->
  {lists:reverse(Acc), S}.

%% In case we have no return, we wrap the expression
%% in a block that returns nil.
wrap_expr(Expr, false) -> {'__block__', [], [Expr, nil]};
wrap_expr(Expr, _)  -> Expr.

translate_gen(_Meta, Left, Right, T, S) ->
  {TRight, SR} = elixir_erl_pass:translate(Right, S),
  {LeftArgs, LeftGuards} = elixir_utils:extract_guards(Left),
  {TLeft, SL} = elixir_erl_clauses:match(fun elixir_erl_pass:translate/2, LeftArgs,
                                         SR#elixir_erl{extra=pin_guard}),

  TLeftGuards = elixir_erl_clauses:guards(LeftGuards, [], SL),
  ExtraGuards = [{nil, X} || X <- SL#elixir_erl.extra_guards],
  SF = SL#elixir_erl{extra=S#elixir_erl.extra, extra_guards=[]},

  {TT, {TFilters, TS}} = translate_filters(T, SF),
build_inline(Ann, Clauses, Expr, Into, Uniq, S) ->
  case not Uniq and lists:all(fun(Clause) -> element(1, Clause) == bin end, Clauses) of
    true  -> {build_comprehension(Ann, Clauses, Expr, Into), S};
    false -> build_inline_each(Ann, Clauses, Expr, Into, Uniq, S)
  end.

build_inline_each(Ann, Clauses, Expr, false, Uniq, S) ->
  InnerFun = fun(InnerExpr, _InnerAcc) -> InnerExpr end,
  {build_reduce(Ann, Clauses, InnerFun, Expr, {nil, Ann}, Uniq, S), S};
build_inline_each(Ann, Clauses, Expr, {nil, _} = Into, Uniq, S) ->
  InnerFun = fun(InnerExpr, InnerAcc) -> {cons, Ann, InnerExpr, InnerAcc} end,
  ReduceExpr = build_reduce(Ann, Clauses, InnerFun, Expr, Into, Uniq, S),
  {elixir_erl:remote(Ann, lists, reverse, [ReduceExpr]), S};
build_inline_each(Ann, Clauses, Expr, {bin, _, []}, Uniq, S) ->
  {InnerValue, SV} = build_var(Ann, S),

  InnerFun = fun(InnerExpr, InnerAcc) ->
    {'case', Ann, InnerExpr, [
      {clause, Ann,
       [InnerValue],
       [[elixir_erl:remote(Ann, erlang, is_binary, [InnerValue]),
         elixir_erl:remote(Ann, erlang, is_list, [InnerAcc])]],
       [{cons, Ann, InnerAcc, InnerValue}]},
      {clause, Ann,
       [InnerValue],
       [[elixir_erl:remote(Ann, erlang, is_bitstring, [InnerValue]),
         elixir_erl:remote(Ann, erlang, is_bitstring, [InnerAcc])]],
       [{bin, Ann, [
          {bin_element, Ann, InnerACc, default, [bitstring]},
          {bin_element, Ann, InnerValue, default, [bitstring]}
       ]}]},
      {clause, Ann,
       [InnerValue],
       [[elixir_erl:remote(Ann, erlang, is_bitstring, [InnerValue])]],
       [{build_var, Ann, [
           {bin_element, Ann, InnerAcc, default, [bitstring]},
           {bin_element, Ann, InnerValue, default, [bitstring]}
       ]}]},
    ]}
comprehension_expr({bin, _, []}, {bin, _, _} = Expr) ->
  {inline, Expr};
comprehension_expr({bin, Ann, []}, Expr) ->
  BinExpr = {bin, Ann, [{bin_element, Ann, Expr, default, [bitstring]}]},
  {inline, BinExpr};
comprehension_expr({nil, _}, Expr) ->
  {inline, Expr};
comprehension_expr(false, Expr) ->
  {inline, Expr};
comprehension_expr(_, Expr) ->
  {into, Expr}.

%% Helpers

build_reduce(Ann, Clauses, InnerFun, Expr, Into, false, S) ->
  {Acc, SA} = build_var(Ann, S),
  build_reduce_each(Clauses, InnerFun(Expr, Acc), Into, Acc, SA);
build_reduce(Ann, Clauses, InnerFun, Expr, Into, true, S) ->
  %% Those variables are used only inside the anonymous function
  %% so we don't need to worry about returning the scope.
  {Acc, SA} = build_var(Ann, S),
  {Value, SV} = build_var(Ann, SA),
  {IntoAcc, SI} = build_var(Ann, SV),
  {UniqAcc, SU} = build_var(Ann, SI),

  NewInto = {tuple, Ann, [Into, {map, Ann, []}]},
  AccTuple = {tuple, Ann, [IntoAcc, UniqAcc]},
  PutUniqExpr = elixir_erl:remote(Ann, maps, put, [Value, {atom, Ann, true}, UniqAcc]),

  InnerExpr = {block, Ann, [
    {match, Ann, AccTuple, Acc},
    {match, Ann, Value, Expr},
    {'case', Ann, UniqAcc, [
      {clause, Ann, [{map, Ann, [{map_field_exact, Ann, Value, {atom, Ann, true}}]}], [], [AccTuple]},
      {clause, Ann, [{map, Ann, []}], [], [{tuple, Ann, [InnerFun(Value, IntoAcc), PutUniqExpr]}]}
    ]}
  ]},

  EnumReduceCall = build_reduce_each(Clauses, InnerExpr, NewInto, Acc, SU),
  elixir_erl:remote(Ann, erlang, element, [{integer, Ann, 1}, EnumReduceCall]).

build_reduce_each([{enum, Meta, Left, Right, Filters} | T], Expr, Arg, Acc, S) ->
  Ann = ?ann(Meta),
  True = build_reduce_each(T, Expr, Acc, Acc, S),
  False = Acc,
  Generated = erl_anno:set_generated(true, Ann),

  Clauses0 =
    case is_var(Left) of
      true  -> [];
      false ->
        [{clause, Generated,
          [{var, Ann, '_'}, Acc], [],
          [False]}]
    end,

  Clauses1 =
    [{clause, Ann,
      [Left, Acc], [],
      [join_filters(Generated, Filters, True, False)]} | Clauses0],

  Args  = [Right, Arg, {'fun', Ann, {clauses, Clauses1}}],
  elixir_erl:remote(Ann, 'Elixir.Enum', reduce, Args);

build_reduce_each([{bin, Meta, Left, Right, Filters} | T], Expr, Arg, Acc, S) ->
  Ann = ?ann(Meta),
  Generated  = erl_anno:set_generated(true, Ann),
  {Tail, ST} = build_var(Ann, S),
  {Fun, SF}  = build_var(Ann, ST),

  True  = build_reduce_each(T, Expr, Acc, Acc, SF),
  False = Acc,

  {bin, _, Elements} = Left,

  BinMatch =
    {bin, Ann, Elements ++ [{bin_element, Ann, Tail, default, [bitstring]}]},
  NoVarMatch =
    {bin, Ann, no_var(Elements) ++ [{bin_element, Ann, Tail, default, [bitstring]}]},

  Clauses =
    [{clause, Ann,
      [BinMatch, Acc], [],
      [{call, Ann, Fun, [Tail, join_filters(Generated, Filters, True, False)]}]},
     {clause, Generated,
      [NoVarMatch, Acc], [],
      [{call, Ann, Fun, [Tail, False]}]},
     {clause, Generated,
      [{bin, Ann, []}, Acc], [],
      [Acc]},
     {clause, Generated,
      [Tail, {var, Ann, '_'}], [],
      [elixir_erl:remote(Ann, erlang, error, [pair(Ann, badarg, Tail)])]}],

  {call, Ann,
    {named_fun, Ann, element(3, Fun), Clauses},
    [Right, Arg]};

build_reduce_each([], Expr, _Arg, _Acc, _S) ->
  Expr.

pair(Ann, Atom, Arg) ->
  {tuple, Ann, [{atom, Ann, Atom}, Arg]}.

build_var(Ann, S) ->
  {Name, _, ST} = elixir_erl_var:build('_', S),
  {{var, Ann, Name}, ST}.

no_var(Elements) ->
  [{bin_element, Ann, no_var_expr(Expr), Size, Types} ||
    {bin_element, Ann, Expr, Size, Types} <- Elements].
no_var_expr({var, Ann, _}) ->
  {var, Ann, '_'}.

join_filters(_Ann, [], True, _False) ->
  True;
join_filters(Ann, [H | T], True, False) ->
  lists:foldl(fun(Filter, Acc) ->
    join_filter(Ann, Filter, Acc, False)
  end, join_filter(Ann, H, True, False), T).

join_filter(Ann, {nil, Filter}, True, False) ->
  {'case', Ann, Filter, [
    {clause, Ann, [{atom, Ann, true}], [], [True]},
    {clause, Ann, [{atom, Ann, false}], [], [False]}
  ]};
join_filter(Ann, {Var, Filter}, True, False) ->
  Guards = [
    [{op, Ann, '==', Var, {atom, Ann, false}}],
    [{op, Ann, '==', Var, {atom, Ann, nil}}]
  ],

  {'case', Ann, Filter, [
    {clause, Ann, [Var], Guards, [False]},
    {clause, Ann, [{var, Ann, '_'}], [], [True]}
  ]}.
