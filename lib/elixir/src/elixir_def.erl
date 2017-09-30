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

%% Take a definition out of the table

take_definition(Module, {Name, Arity} = Tuple) ->
  Table = elixir_module:defs_table(Module),
  case ets:take(Table, {def, Tuple}) of
    [{{def, Tuple}, _, _, _, _, {Defaults, _, _}} = Result] ->
      ets:delete_object(Table, {{default, Name}, Arity, Defaults}),
      {Result, [Clause || {_, Clause} <- ets:take(Table, {clauses, Tuple})]};
    [] ->
      false
  end.

%% Fetch all available definitions

fetch_definitions(File, Module) ->
  Table = elixir_module:defs_table(Module),
  Entries = ets:match(Table, {{def, '$1'}, '_', '_', '_', '_', '_'}),
  {All, Private} = fetch_definition(lists:sort(Entries), File, Module, Table, [], []),
  Unreachable = elixir_locals:warn_unused_local(File, Module, Private),
  elixir_locals:ensure_no_import_conflict(File, Module, All),
  {All, Unreachable}.

fetch_definition([[Tuple] | T], File, Module, Table, All, Private) ->
  [{_, Kind, Meta, _, Check, {Defaults, _, _}}] = ets:lookup(Table, {def, Tuple}),

  try ets:lookup_element(Table, {clauses, Tuple}, 2) of
    Clauses ->
      NewAll =
        [{Tuple, Kind, Meta, Clauses} | All],
      NewPrivate =
        case (Kind == defp) orelse (Kind == defmacrop) of
          true ->
            WarnMeta = case Check of true -> Meta; false -> false end,
            [{Tuple, Kind, WarnMeta, Defaults} | Private];
          false ->
            Private
        end,
      fetch_definition(T, File, Module, Table, NewAll, NewPrivate)
  catch
    error:badarg ->
      warn_bodyless_function(Check, Meta, File, Module, Kind, Tuple),
      fetch_definition(T, File, Module, Table, All, Private)
  end;

fetch_definition([], _File, _Module, _Table, All, Private) ->
  {All, Private}.

%% Section for storing definitions

store_definition(Kind, CheckClauses, Call, Body, Pos) ->
  #{line := Line} = E = elixir_locals:get_cached_env(Pos),
  {NameAndArgs, Guards} = elixir_utils:extract_guards(Call),

  {Name, Args} = case NameAndArgs of
    {N, _, A} when is_atom(N), is_atom(A) -> {N, []};
    {N, _, A} when is_atom(N), is_list(A) -> {N, A};
    _ -> elixir_errors:form_error([{line, Line}], ?key(E, file), ?MODULE,
                                  {invalid_def, Kind, NameAndArgs})
  end,

  %% Now that we have verified the call format,
  %% extract meta information like file and context.
  {_, Meta, _} = Call,
  DoCheckClauses = (not lists:keymember(context, 1, Meta)) andalso (CheckClauses),

  %% Check if there is a file information in the definition.
  %% If so, we assume this come from another source and
  %% we need to linify taking into account keep line numbers.
  %%
  %% Line and File will always point to the caller. __ENV__.line
  %% will always point to the quoted one and __ENV__.file will
  %% always point to the one at @file or the quoted one.
  {Location, Key} =
    case elixir_utils:meta_location(Meta) of
      {_, _} = Keep -> {Keep, keep};
      nil -> {nil, line}
    end,

  Arity        = length(Args),
  LinifyArgs   = elixir_quote:linify(Line, Key, Args),
  LinifyGuards = elixir_quote:linify(Line, Key, Guards),
  LinifyBody   = elixir_quote:linify(Line, Key, Body),
  Generated    = case DoCheckClauses of true -> []; false -> ?generated([]) end,

  {EL, MetaLocation} =
    case retrieve_location(Location, ?key(E, module)) of
      {F, L} ->
        {E#{file := F}, [{line, Line}, {location, {F, L}} | Generated]};
      nil ->
        {E, [{line, Line} | Generated]}
    end,

  assert_no_aliases_name(MetaLocation, Name, Args, EL),
  assert_valid_name(MetaLocation, Kind, Name, Args, EL),
  store_definition(MetaLocation, Kind, DoCheckClauses, Name, Arity,
                   LinifyArgs, LinifyGuards, LinifyBody, ?key(E, file), EL).

store_definition(Meta, Kind, CheckClauses, Name, Arity, DefaultsArgs, Guards, Body, File, ER) ->
  Module = ?key(ER, module),
  Tuple = {Name, Arity},
  E = ER#{function := Tuple},

  elixir_locals:record_definition(Tuple, Kind, Module),
  {Args, Defaults} = unpack_defaults(Kind, Meta, Name, DefaultsArgs, E),
  Clauses = [elixir_clauses:def(Clause, E) ||
             Clause <- def_to_clauses(Kind, Meta, Args, Guards, Body, E)],

  DefaultsLength = length(Defaults),
  elixir_locals:record_defaults(Tuple, Kind, Module, DefaultsLength),
  check_previous_defaults(Meta, Module, Name, Arity, Kind, DefaultsLength, E),
  run_on_definition_callbacks(Kind, Module, Name, DefaultsArgs, Guards, Body, E),

  store_definition(CheckClauses, Kind, Meta, Name, Arity, File,
                   Module, DefaultsLength, Clauses),
  [store_definition(false, Kind, Meta, Name, length(DefaultArgs), File,
                    Module, 0, [Default]) || {_, DefaultArgs, _, _} = Default <- Defaults],
  Tuple.

retrieve_location(Location, Module) ->
  case ets:take(elixir_module:data_table(Module), file) of
    [] when is_tuple(Location) ->
      {File, Line} = Location,
      {elixir_utils:relative_to_cwd(File), Line};
    [] ->
      nil;
    [{file, File, _, _}] when is_binary(File) ->
      'Elixir.Module':delete_attribute(Module, file),
      {elixir_utils:relative_to_cwd(File), 0};
    [{file, {File, Line}, _, _}] when is_binary(File) andalso is_integer(Line) ->
      'Elixir.Module':delete_attribute(Module, file),
      {elixir_utils:relative_to_cwd(File), Line}
  end.

def_to_clauses(_Kind, Meta, Args, [], nil, E) ->
  check_args_for_bodyless_clause(Meta, Args, E),
  [];
def_to_clauses(Kind, Meta, _Args, _Guards, nil, E) ->
  elixir_errors:form_error(Meta, ?key(E, file), elixir_expand, {missing_option, Kind, [do]});
def_to_clauses(_Kind, Meta, Args, Guards, [{do, Body}], _E) ->
  [{Meta, Args, Guards, Body}];
def_to_clauses(Kind, Meta, Args, Guards, Body, _E) ->
  [{Meta, Args, Guards, {'try', [{origin, Kind} | Meta], [Body]}}].

run_on_definition_callbacks(Kind, Module, Name, Args, Guards, Body, E) ->
  Callbacks = ets:lookup_element(elixir_module:data_table(Module), on_definition, 2),
  _ = [Mod:Fun(E, Kind, Name, Args, Guards, Body) || {Mod, Fun} <- Callbacks],
  ok.

store_definition(Check, Kind, Meta, Name, Arity, File, Module, Befaults, Clauses) ->
  Data = elixir_module:data_table(Module),
  Defs = elixir_module:defs_table(Module),

  Tuple   = {Name, Arity},
  HasBody = Clauses =/= [],

  if
    Defaults > 0 ->
      ets:insert(Defs, {{default, Name}, Arity, Defaults});
    true ->
      ok
  end,

  MaxDefaults =
    case ets:take(Defs, {def, Tuple}) of
      [{_, StoredKind, StoredMeta, StoredFile, StoredCheck,
          {StoredDefaults, LastHasBody, LastDefaults}}] ->
        check_valid_kind(Meta, File, Name, Arity, Kind, StoredKind),
        (Check and StoredCheck) andalso
          check_valid_clause(Meta, File, Name, Arity, Kind, Data, StoredMeta, StoredFile),
          check_valid_defaults(Meta, File, Name, Arity, Kind, Defaults, StoredDefaults, LastDefaults, LastHasBody),
          max(Defaults, StoredDefaults);
      [] ->
        Defaults
    end,

  Check andalso ets:insert(Data, {?last_def, Tuple}),
  ets:insert(Defs, [{{clauses, Tuple}, Clause} || Clause <- Clauses]),
  ets:insert(Defs, {{def, Tuple}, Kind, Meta, File, Check, {MaxDefaults, HasBody, Defaults}}).

%% Handling of defaults

unpack_defaults(Kind, Meta, Name, Args, E) ->
  Expanded = expand_defaults(Args, E#{context := nil}),
  unpack_defaults(Kind, Meta, Name, Expanded, [], []).



