-module(elixir_erl_compiler).
-export([spawn/2, forms/3, noenv_forms/3, erl_to_core/2, format_error/1]).
-include("elixir.hrl").

spawn(Fun, Args) ->
  CompilerPid = get(elixir_compiler_pid),

  {_, Ref} =
    spawn_monitor(fun() ->
      put(elixir_compiler_pid, CompilerPid),

      try apply(Fun, Args) of
        Result -> exit({ok, Result})
      catch
        ?WITH_STACKTRACE(Kind, Reason, Stack)
          exit({Kind, Reason, Stack})
      end
    end),

  receive
    {'DOWN', Ref, process, _, {ok, Result}} ->
      Result;
    {'DOWN', Ref, process, _, {Kind, Reason, Stack}} ->
      erlang:raise(Kind, Reason, Stack)
  end.

forms(Forms, File, Opts) ->
  compile(Forms, File, Opts ++ compile:env_compiler_options()).

noenv_forms(Forms, File, Opts) ->
  compile(Forms, File, Opts).

erl_to_core(Forms, Opts) ->
  %% TODO: Remove parse transform handling on Elixir v2.0
  case [M || {parse_transform, M} <- Opts] of
    [] ->
      v3_core:module(Forms, Opts);
    _ ->
      case compile:noenv_forms(Forms, [?NO_SPAWN_COMPILER_PROCESS, to_core0, return, no_auto_import | Opts]) of
        {ok, _Module, Core, Warnings} -> {ok, Core, Warnings};
        {error, Errors, Warnings} -> {error, Errors, Warnings}
      end
  end.

compile(Forms, File, Opts) when is_list(Forms), is_list(Opts), is_binary(File) ->
  Source = elixir_utils:characters_to_list(File),

  case erl_to_core(Forms, Opts) of
    {ok, CoreForms, CoreWarnings} ->
      format_warnings(Opts, CoreWarnings),

      case compile:noenv_forms(CoreForms, [?NO_SPAWN_COMPILER_PROCESS, from_core, no_auto_import, return, {source, Source} | Opts]) of
        {ok, Module, Binary, Warnings} when is_binary(Binary) ->
          format_warnings(Opts, Warnings),
          {Module, Binary};

        {ok, Module, _Binary, _Warnings} ->
          elixir_errors:form_error([], File, ?MODULE, {invalid_compilation, Module});

        {error, Errors, Warnings} ->
          format_warnings(Opts, Warnings),
          format_errors(Errors)
      end;

    {error, CoreErrors, CoreWarnings} ->
      format_warnings(Opts, CoreWarnings),
      format_errors(CoreErrors)
  end.

format_errors([]) ->
  exit({nocompile, "compilation failed but no error was raised"});
format_errors(Errors) ->
  lists:foreach(fun ({File, Each}) ->
    BinFile = elixir_utils:characters_to_binary(File),
    lists:foreach(fun(Error) -> handle_file_error(BinFile, Error) end, Each)
  end, Errors).

format_warnings(Opts, Warnings) ->
  NoWarnNoMatch = proplists:get_value(nowarn_nomatch, Opts, false),
  lists:foreach(fun ({File, Each}) ->
    BinFile = elixir_utils:characters_to_binary(File),
    lists:foreach(fun(Warning) ->
      handle_file_warning(NoWarnNoMatch, BinFile, Warning)
    end, Each)
  end, Warnings).

%% Handle warnings from Erlang land

%% Ignore nomatch warnings
handle_file_warning(true, _File, {_Line, sys_core_fold, nomatch_guard}) -> ok;
handle_file_warning(true, _File, {_Line, sys_core_fold, {nomatch_shadow, _}}) -> ok;

%% Those we implement ourselves
handle_file_warning(_, _File, {_Line, v3_core, {map_key_repeated, _}}) -> ok;
handle_file_warning(_, _File, {_Line, sys_core_fold, useless_building}) -> ok;
