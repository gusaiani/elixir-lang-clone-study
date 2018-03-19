%% Translate Elixir quoted expressions to Erlang Abstract Format
-module(elixir_erl_pass).
-export([translate/2, translate_arg/3, translate_args/2]).
-import(elixir_erl_var, [mergev/2, mergec/2]).
-include("elixir.hrl").

%% =

translate({'=', Meta, [{'_', _, Atom}, Right]}, S) when is_atom(Atom) ->
  {TRight, SR} = translate(Right, S),
  {{match, ?ann(Meta), {var, ?ann(Meta), '_'}, TRight}, SR};

translate({'=', Meta, [Left, Right]}, S) ->
  {TRight, SR} = translate(Right, S),
  case elixir_erl_clauses:match(fun translate/2, Left, SR) of
    {TLeft, #elixir_erl{extra_guards=ExtraGuards, context=Context} = SL0}
        when ExtraGuards =/= [], Context =/= match ->
      SL1 = SL0#elixir_erl{extra_guards=[]},
      Match = {match, ?ann(Meta), TLeft, TRight},
      Generated = ?ann(?generated(Meta)),
      {ResultVarName, _, SL2} = elixir_erl_var:build('_', SL1),
      ResultVar = {var, Generated, ResultVarName},
      ResultMatch = {match, Generated, ResultVar, Match},
      True = {atom, Generated, true},
      Reason = {tuple, Generated, [{atom, Generated, badmatch}, ResultVar]},
      RaiseExpr = elixir_erl:remote(Generated, erlang, error, [Reason]),
      GuardsExp = {'if', Generated, [
        {clause, Generated, [], [ExtraGuards], [ResultVar]},
        {clause, Generated, [], [[True]], [RaiseExpr]}
      ]},
      {{block, Generated, [ResultMatch, GuardsExp]}, SL2};

    {TLeft, SL} ->
      {{match, ?ann(Meta), TLeft, TRight}, SL}
  end;

