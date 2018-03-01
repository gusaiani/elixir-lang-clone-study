%% A bunch of helpers to help to deal with errors in Elixir source code.
%% This is not exposed in the Elixir language.
%%
%% Notice this is also called by the Erlang backend, so we also support
%% the line number to be none (as it may happen in some erlang errors).
-module(elixir_errors).
-export([compile_error/3, compile_error/4,
         form_error/4, form_warn/4, parse_error/4, bare_warn/3, warn/3]).
-include("elixir.hrl").

-spec warn(non_neg_integer() | none, unicode:chardata(), unicode:chardata()) -> ok.
warn(none, File, Warning) ->
  warn(0, File, Warning).
warn(Line, File, Warning) when is_integer(Line), is_binary(File)->
  send_warning(File, Line, Warning),
  print_warning([Warning, "\n  ", file_format(Line, File), $\n]).

file_format(0, File) ->
  io_lib:format("~ts", [elixir_utils:relative_to_cwd(File)]);

file_format(Line, File) ->
  io_lib:format("~ts:~w", [elixir_utils:relative_to_cwd(File), Line]).
