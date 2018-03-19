%% Translate Elixir quoted expressions to Erlang Abstract Format
-module(elixir_erl_pass).
-export([translate/2, translate_arg/3, translate_args/2]).
-import(elixir_erl_var, [mergev/2, mergec/2]).
-include("elixir.hrl").

%% =

translate({'=', Meta, [{'_', _, Atom}, Right]}, S) when is_atom(Atom) ->
  {TRight, SR} = translate(Right, S),
  {{match, ?ann(Meta), {var, ?ann(Meta), '_'}, TRight}, SR};
