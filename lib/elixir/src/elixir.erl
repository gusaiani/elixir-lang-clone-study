%% Main entry point for Elixir functions. All of those functions are
%% private to the Elixir compiler and reserved to be used by Elixir only.
-module(elixir).
-behaviour(application).
-export([start_cli/0,
  string_to_quoted/4, 'string_to_quoted!'/4,
  env_for_eval/1, env_for_eval/2, quoted_to_erl/2, quoted_to_erl/3,
  eval/2, eval/3, eval_forms/3, eval_forms/4, eval_quoted/3]).
-include("elixir.hrl").
-define(system, 'Elixir.System').

%% Top level types
%% TODO: Deprecate char_list type by v1.5
-export_type([charlist/0, char_list/0, struct/0, as_boolean/1, keyword/0, keyword/1]).
-type charlist() :: string().
-type char_list() :: string().
-type as_boolean(T) :: T.
-type keyword() :: [{atom(), any()}].
-type keyword(T) :: [{atom(), T}].

-ifdef(old_map_specs).
-type struct() :: #{'__struct__' => atom(), atom() => any()}.
-else.
-type struct() :: #{'__struct__' := atom(), atom() => any()}.
-endif.

%% OTP Application API

-export([start/2, stop/1, config_change/3]).

start(_Type, _Args) ->
  %% In case there is a shell, we can't really change its
  %% encoding, so we just set binary to true. Otherwise
  %% we must set the encoding as the user with no shell
  %% has encoding set to latin1.
  Opts =
    case init:get_argument(noshell) of
      {ok, _} -> [binary, {encoding, utf8}];
      error   -> [binary]
    end,

  case string:to_integer(erlang:system_info(otp_release)) of
    {Num, _} when Num >= 18 ->
      ok;
    _ ->
      io:format(standard_error, "unsupported Erlang version, expected Erlang 18+~n", []),
      erlang:halt(1)
  end,

  case code:ensure_loaded(?system) of
    {module, ?system} ->
      Endianness = ?system:endianness(),
      case ?system:compiled_endianness() of
        Endianness -> ok;
        _ ->
          io:format(standard_error,
            "warning: Elixir is running in a system with a different endianness than the one its "
            "source code was compiled in. Please make sure Elixir and all source files were compiled "
            "in a machine with the same endianness as the current one: ~ts~n", [Endianness])
      end;
    {error, _} ->
      ok
  end,

  ok = io:setopts(standard_io, Opts),
  ok = io:setopts(standard_error, [{encoding, utf8}]),

  Encoding = file:native_name_encoding(),
  case Encoding of
    latin1 ->
      io:format(standard_error,
        "warning: the VM is running with native name encoding of latin1 which may cause "
        "Elixir to malfunction as it expects utf8. Please ensure your locale is set to UTF-8 "
        "(which can be verified by running \"locale\" in your shell)~n", []);
    _ ->
      ok
  end,

  URIConfig = [{{uri, <<"ftp">>}, 21},
               {{uri, <<"sftp">>}, 22},
               {{uri, <<"tftp">>}, 69},
               {{uri, <<"http">>}, 80},
               {{uri, <<"https">>}, 443},
               {{uri, <<"ldap">>}, 389}],
  CompilerOpts = #{docs => true, ignore_module_conflict => false,
                   debug_info => true, warnings_as_errors => false,
                   relative_paths => true},
  {ok, [[Home] | _]} = init:get_argument(home),
  Config = [{at_exit, []},
            {home, unicode:characters_to_binary(Home, Encoding, Encoding)},
            {compiler_options, CompilerOpts}
            | URIConfig],
  Tab = elixir_config:new(Config),
  case elixir_sup:start_link() of
    {ok, Sup} ->
      {ok, Sup, Tab};
    {error, _Reason} = Error ->
      elixir_config:delete(Tab),
      Error
  end.

stop(Tab) ->
  elixir_config:delete(Tab).

config_change(_Changed, _New, _Remove) ->
  ok.

%% Boot and process given options. Invoked by Elixir's script.

start_cli() ->
  {ok, _} = application:ensure_all_started(?MODULE),

  %% We start the Logger so tools that depend on Elixir
  %% always have the Logger directly accessible. However
  %% Logger is not a dependency of the Elixir application,
  %% which means releases that want to use Logger must
  %% always list it as part of its applications.
  _ = case code:ensure_loaded('Elixir.Logger') of
    {module, _} -> application:start(logger);
    {error, _}  -> ok
  end,

  'Elixir.Kernel.CLI':main(init:get_plain_arguments()).

%% EVAL HOOKS

env_for_eval(Opts) ->
  env_for_eval((elixir_env:new())#{
    requires := elixir_dispatch:default_requires(),
    functions := elixir_dispatch:default_functions(),
    macros := elixir_dispatch:default_macros()
 }, Opts).

env_for_eval(Env, Opts) ->
  Line = case lists:keyfind(line, 1, Opts) of
    {line, LineOpt} when is_integer(LineOpt) -> LineOpt;
    false -> ?m(Env, line)
  end,

  File = case lists:keyfind(file, 1, Opts) of
    {file, FileOpt} when is_binary(FileOpt) -> FileOpt;
    false -> ?m(Env, file)
  end,

  Aliases = case lists:keyfind(aliases, 1, Opts) of
    {aliases, AliasesOpt} when is_list(AliasesOpt) -> AliasesOpt;
    false -> ?m(Env, aliases)
  end,

  Requires = case lists:keyfind(requires, 1, Opts) of
    {requires, RequiresOpt} when is_list(RequiresOpt) -> ordsets:from_list(RequiresOpt);
    false -> ?m(Env, requires)
  end,

  Functions = case lists:keyfind(functions, 1, Opts) of
    {functions, FunctionsOpt} when is_list(FunctionsOpt) -> FunctionsOpt;
    false -> ?m(Env, functions)
  end,

  Macros = case lists:keyfind(macros, 1, Opts) of
    {macros, MacrosOpt} when is_list(MacrosOpt) -> MacrosOpt;
    false -> ?m(Env, macros)
  end,

  Module = case lists:keyfind(module, 1, Opts) of
    {module, ModuleOpt} when is_atom(ModuleOpt) -> ModuleOpt;
    false -> nil
  end,

  Env#{
    file := File, module := Module,
    macros := Macros, functions := Functions,
    requires := Requires, aliases := Aliases, line := Line
  }.

%% String evaluation

eval(String, Binding) ->
  eval(String, Binding, []).

eval(String, Binding, Opts) when is_list(Opts) ->
  eval(String, Binding, env_for_eval(Opts));
eval(String, Binding, #{line := Line, file := File} = E) when
    is_list(String), is_list(Binding), is_integer(Line), is_binary(File) ->
  Forms = 'string_to_quoted!'(String, Line, File, []),
  eval_forms(Forms, Binding, E).

%% Quoted evaluation

eval_quoted(Tree, Binding, Opts) when is_list(Opts) ->
  eval_quoted(Tree, Binding, env_for_eval(Opts));
eval_quoted(Tree, Binding, #{line := Line} = E) ->
  eval_forms(elixir_quote:linify(Line, Tree), Binding, E).

%% Handle forms evaluation. The main difference to
%% eval_quoted is that it does not linefy the given
%% args.

eval_forms(Tree, Binding, Opts) when is_list(Opts) ->
  eval_forms(Tree, Binding, env_for_eval(Opts));
eval_forms(Tree, Binding, E) ->
  eval_forms(Tree, Binding, E, elixir_env:env_to_scope(E)).
eval_forms(Tree, Binding, Env, Scope) ->
  {ParsedBinding, ParsedVars, ParsedScope} = elixir_scope:load_binding(Binding, Scope),
  ParsedEnv = Env#{vars := ParsedVars},
  {Erl, NewEnv, NewScope} = quoted_to_erl(Tree, ParsedEnv, ParsedScope),

  case Erl of
    {atom, _, Atom} ->
      {Atom, Binding, NewEnv, NewScope};
    _  ->
      {value, Value, NewBinding} = erl_eval(Erl, ParsedBinding, Env),
      {Value, elixir_scope:dump_binding(NewBinding, NewScope), NewEnv, NewScope}
  end.

erl_eval(Erl, ParsedBinding, E) ->
  case erl_eval:check_command([Erl], ParsedBinding) of
    ok -> ok;
    {error, Desc} -> elixir_errors:handle_file_error(?m(E, file), Desc)
  end,

  % Below must be all one line for locations to be the same when the stacktrace
  % needs to be extended to the full stacktrace.
  try erl_eval:expr(Erl, ParsedBinding, none, none, none) catch Class:Exception -> erlang:raise(Class, Exception, get_stacktrace()) end.

get_stacktrace() ->
  Stacktrace = erlang:get_stacktrace(),
  % eval_eval and eval_bits can call :erlang.raise/3 without the full
  % stacktrace. When this occurs re-add the current stacktrace so that no
  % stack information is lost.
  try
    throw(stack)
  catch
    throw:stack ->
      % Ignore stack item for current function.
      [_ | CurrentStack] = erlang:get_stacktrace(),
      get_stacktrace(Stacktrace, CurrentStack)
  end.

% The stacktrace did not include the current stack, re-add it.
get_stacktrace([], CurrentStack) ->
  CurrentStack;
% The stacktrace includes the current stack.
get_stacktrace(CurrentStack, CurrentStack) ->
  CurrentStack;
get_stacktrace([StackItem | Stacktrace], CurrentStack) ->
  [StackItem | get_stacktrace(Stacktrace, CurrentStack)].

%% Converts a quoted expression to Erlang abstract format

quoted_to_erl(Quoted, Env) ->
  quoted_to_erl(Quoted, Env, elixir_env:env_to_scope(Env)).

quoted_to_erl(Quoted, Env, Scope) ->
  {Expanded, NewEnv} = elixir_exp:expand(Quoted, Env),
  {Erl, NewScope}    = elixir_translator:translate(Expanded, Scope),
  {Erl, NewEnv, NewScope}.

%% Converts a given string (charlist) into quote expression

string_to_quoted(String, StartLine, File, Opts) when is_integer(StartLine), is_binary(File) ->
  case elixir_tokenizer:tokenize(String, StartLine, [{file, File} | Opts]) of
    {ok, _Line, _Column, Tokens} ->
      put(elixir_parser_file, File),
      try elixir_parser:parse(Tokens) of
        {ok, Forms} -> {ok, Forms};
        {error, {{Line, _, _}, _, [Error, Token]}} -> {error, {Line, to_binary(Error), to_binary(Token)}};
        {error, {Line, _, [Error, Token]}} -> {error, {Line, to_binary(Error), to_binary(Token)}}
      catch
        {error, {{Line, _, _}, _, [Error, Token]}} -> {error, {Line, to_binary(Error), to_binary(Token)}};
        {error, {Line, _, [Error, Token]}} -> {error, {Line, to_binary(Error), to_binary(Token)}}
      after
        erase(elixir_parser_file)
      end;
    {error, {Line, {ErrorPrefix, ErrorSuffix}, Token}, _Rest, _SoFar} ->
      {error, {Line, {to_binary(ErrorPrefix), to_binary(ErrorSuffix)}, to_binary(Token)}};
    {error, {Line, Error, Token}, _Rest, _SoFar} ->
      {error, {Line, to_binary(Error), to_binary(Token)}}
  end.

'string_to_quoted!'(String, StartLine, File, Opts) ->
  case string_to_quoted(String, StartLine, File, Opts) of
    {ok, Forms} ->
      Forms;
    {error, {Line, Error, Token}} ->
      elixir_errors:parse_error(Line, File, Error, Token)
  end.

to_binary(List) when is_list(List) -> elixir_utils:characters_to_binary(List);
to_binary(Atom) when is_atom(Atom) -> atom_to_binary(Atom, utf8).