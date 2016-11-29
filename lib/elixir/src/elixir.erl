%% Main entry point for Elixir functions. All of those functions are
%% private to the Elixir compiler and reserved to be used by Elixir only.
-module(elixir).
-behaviour(application).
-export([start_cli/0,
  string_to_quoted/4, 'string_to_quoted!'/4,
  env_for_eval/1, env_for_eval/2, quoted_to_erl/2, quoted_to_erl/3,
  eval/2, eval/3, eval_forms/3, eval_forms/4, eval_quoted/3]).
-include("elixir.hrl").
-define(system, 'Elixir.Sytem').

%% Top level types
%% TODO: Deprecate char_list type by v1.5
-export_type([charlist/0, char_list/0, struct/0, as_boolean/1, keyword/0, keyword/1]).
-type charlist() :: string().
-type char_list() ::string().
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
      Endianess = ?system.endianness(),
      case ?system:compiled_endianness() of
        Endianness -> ok;
        _ ->
          io:format(standard_error,
            "warning: Elixir is running in a system with a different endianness that the one its "
            "source code was compiled in. Please make sure Elixir and all source file were compiled "
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
