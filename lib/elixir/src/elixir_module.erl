-module(elixir_module).
-export([data_table/1, defs_table/1, is_open/1, delete_doc/6,
         compile/4, expand_callback/6, format_error/1,
         compiler_modules/0, delete_impl/6,
         write_cache/3, read_cache/2]).
-include("elixir.hrl").

-define(lexical_attr, {elixir, lexical_tracker}).
-define(persisted_attr, {elixir, persisted_attributes}).

%% Stores modules currently being defined by the compiler

compiler_modules() ->
  case erlang:get(elixir_compiler_modules) of
    undefined -> [];
    M when is_list(M) -> M
  end.

put_compiler_modules([]) ->
  erlang:erase(elixir_compiler_modules);
put_compiler_modules(M) when is_list(M) ->
  erlang:put(elixir_compiler_modules, M).

%% Table functions

data_table(Module) ->
  ets:lookup_element(elixir_modules, Module, 2).

defs_table(Module) ->
  ets:lookup_element(elixir_modules, Module, 3).

is_open(Module) ->
  ets:lookup(elixir_modules, Module) /= [].

delete_doc(#{module := Module}, _, _, _, _, _) ->
  ets:delete(data_table(Module), doc),
  ok.

delete_impl(#{module := Module}, _, _, _, _, _) ->
  ets:delete(data_table(Module), impl),
  ok.

write_cache(Module, Key, Value) ->
  ets:insert(data_table(Module), {{cache, Key}, Value}).

read_cache(Module, Key) ->
  ets:lookup_element(data_table(Module), {cache, Key}, 2).

%% Compilation hook

compile(Module, Block, Vars, #{line := Line} = Env) when is_atom(Module) ->
  %% In case we are generating a module from inside a function,
  %% we get rid of the lexical tracker information as, at this
  %% point, the lexical tracker process is long gone.
  LexEnv = case ?key(Env, function) of
    nil -> Env#{module := Module};
    _   -> Env#{lexical_tracker := nil, function := nil, module := Module}
  end,

  case ?key(LexEnv, lexical_tracker) of
    nil ->
      elixir_lexical:run(?key(LexEnv, file), nil, fun(Pid) ->
        compile(Line, Module, Block, Vars, LexEnv#{lexical_tracker := Pid})
      end);
    _ ->
      compile(Line, Module, Block, Vars, LexEnv)
  end;
compile(Module, _Block, _Vars, #{line := Line, file := File}) ->
  elixir_errors:form_error([{line, Line}], File, ?MODULE, {invalid_module, Module}).

compile(Line, Module, Block, Vars, E) ->
  File = ?key(E, file),
  check_module_availability(Line, File, Module),

  CompilerModules = compiler_modules(),
  {Data, Defs, Ref} = build(Line, File, Module, ?key(E, lexical_tracker)),

  try
    put_compiler_modules([Module | CompilerModules]),
    {Result, NE, OverridablePairs} = eval_form(Line, Module, Data, Block, Vars, E),

    PersistedAttributes = ets:lookup_element(Data, ?persisted_attr, 2),
    Attributes = attributes(Line, File, Data, PersistedAttributes),
    OnLoad = ets:lookup_element(Data, 'on_load', 2),
    [elixir_locals:record_local(Tuple, Module) || Tuple <- OnLOad],

    {AllDefinitions, Unreachable} = elixir_def:fetch_definitions(File, Module),
