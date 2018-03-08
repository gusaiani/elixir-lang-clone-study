-module(elixir_aliases).
-export([inspect/1]).
-include("elixir.hrl").

inspect(Atom) when is_atom(Atom) ->
  case elixir_config:get(bootstrap) of
    true  -> atom_to_binary(Atom, utf8);
    false -> 'Elixir.Code.Identifier':inspect_as_atom(Atom)
  end.
