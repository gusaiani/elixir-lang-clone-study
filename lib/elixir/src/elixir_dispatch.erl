%% Helpers related to dispatching to imports and references.
%% This module access the information stored on the scope
%% by elixir_import and therefore assumes it is normalized (ordsets)
-module(elixir_dispatch).
-export([
  require_function/5, import_function/4
  ]).
-include("elixir.hrl").
-define(kernel, 'Elixir.Kernel').

%% Function retrieval

import_function(Meta, Name, Arity, E) ->
  Tuple = {Name, Arity},
  case find_dispatch(Meta, Tuple, [], E) of
    {function, Receiver} ->
      elixir_lexical:record_import

require_function(Meta, Receiver, Name, Arity, E) ->
  Required = is_element(Receiver, ?key(E, requires)),
  case is_element({Name, Arity}, get_macros(Receiver, Required)) of
    true  -> false;
    false ->
      elixir_lexical:record_remote(Receiver, ?key(E, function), ?key(E, lexical_tracker)),
      remote_function()

remote_function(Meta, Receiver, Name, Arity, E) ->
  check_deprecation(Meta, Receiver, Name, Arity, E),

  case elixir_rewrite:inline(Receiver, Name, Arity) of
    {AR, AN} -> {remote, AR, AN, Arity};
    false    -> {remote, Receiver, Name, Arity}
  end.

%% Helpers

find_dispatch(Meta, Tuple, Extra, E) ->
  case is_import(Meta) of
    {import, _} = Import ->
      Import;
    false ->
      Funs = ?key(E, functions),
      Macs = Extra ++ ?key(E, macros),
      FunMatch = find_dispatch(Tuple, Funs),
      MacMatch = find_dispatch(Tuple, Macs),

      case {FunMatch, MacMatch} of
        {[], [Receiver]} -> {macro, Receiver};
        {[Receiver], []} -> {function, Receiver};
        {[], []} -> false;
        _ ->
          {Name, Arity} = Tuple,
          [First, Second | _] = FunMatch ++ MacMatch,
          Error = {ambiguous_call, {First, Second, Name, Arity}},
          elixir_errors:form_error(Meta, ?key(E, file), ?MODULE, Error)
      end
  end.

find_dispatch(Tuple, List) ->
  [Receiver || {Receiver, Set} <- List, is_element(Tuple, Set)].


is_import(Meta) ->
  case lists:keyfind(import, 1, Meta) of
    {import, _} = Import ->
      case lists:keyfind(context, 1, Meta) of
        {context, _} -> Import;
        false -> false
      end;
    false -> false
  end.

check_deprecation(Meta, Receiver, Name, Arity, #{file := File}) ->
  case deprecation(Receiver, Name, Arity) of
    false -> ok;
    Message ->
      Warning = deprecation_message(Receiver, Name, Arity, Message),
      elixir_errors:warn(?line(Meta), File, Warning)
  end.

deprecation_message(Receiver, '__using__', _Arity, Message) ->
  Warning = io_lib:format("use ~s is deprecated", [elixir_aliases:inspect(Receiver)]),
  deprecation_message(Warning, Message);

deprecation_message(Receiver, Name, Arity, Message) ->
  Warning = io_lib:format("~s.~s/~B is deprecated",
                          [elixir_aliases:inspect(Receiver), Name, Arity]),
  deprecation_message(Warning, Message).

deprecation_message(Warning, Message) ->
  case Message of
    true -> Warning;
    Message -> Warning ++ ", " ++ Message
  end.

%% Modules
deprecation('Elixir.Dict', _, _) ->
  "use the Map module for working with maps or the Keyword module for working with keyword lists";
deprecation('Elixir.GenEvent', _, _) ->
  "use one of the alternatives described in the documentation for the GenEvent module";
deprecation('Elixir.HashDict', _, _) ->
  "use maps and the Map module instead";
deprecation('Elixir.HashSet', _, _) ->
  "use the MapSet module instead";
deprecation('Elixir.Set', _, _) ->
  "use the MapSet module for working with sets";

