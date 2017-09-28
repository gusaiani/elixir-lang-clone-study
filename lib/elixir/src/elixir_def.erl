% Holds the logic responsible for function definitions (def(p) and defmacro(p)).
-module(elixir_def).
-export([setup/1, reset_last/1, local_for/4,
  take_definition/2, store_definition/5, store_definition/9,
  fetch_definitions/2, format_error/1]).
-include("elixir.hrl").
-define(last_def, {elixir, last_def}).

setup(Module) ->
  reset_last(Module),
  ok.

reset_last(Module) ->
  ets:insert(elixir_module:data_table(Module), {?last_def, []}).

local_for(Module, Name, Arity, Kinds) ->
  Tuple = {Name, Arity},

  try
    Table = elixir_module:defs_table(Module),
    {ets:lookup(Table, {def, Tuple}), ets:lookup(Table, {clauses, Tuple})}
  of
    {[{_, Kind, Meta, File, _, _}], Clauses} ->
      case (Kinds == all) orelse (lists:member(Kind, Kinds)) of
        true -> elixir_erl:definition_to_anonymous(File, Module, Kind, Meta,
                                                   [Clause || {_, Clause} <- Clauses]);
        false -> false
      end;
    {[], _} ->
      false
  catch
    _:_ -> false
  end.
