-module(elixir_quote).
-export([escape/3, linify/3, linify_with_context_counter/3, build/6, quote/6, has_unquotes/1, fun_to_quoted/1]).
-export([dot/5, tail_list/3, list/2, validated_runtime/2]). %% Quote callbacks

-include("elixir.hrl").
-define(defs(Kind), Kind == def; Kind == defp; Kind == defmacro; Kind == defmacrop; Kind == '@').
-define(lexical(Kind), Kind == import; Kind == alias; Kind == require).
-compile({inline, [keyfind/2, keystore/3, keydelete/2, keynew/3, do_tuple_linify/5]}).

-record(elixir_quote, {
  line=false,
  file=nil,
  context=nil,
  vars_hygiene=true,
  aliases_hygiene=true,
  imports_hygiene=true,
  unquote=true,
  generated=false
}).

build(Meta, Line, File, Context, Unquote, Generated) ->
  Acc0 = [],
  {ELine, Acc1} = validate_compile(Meta, line, Line, Acc0),

validate_compile(_Meta, line, Value, Acc) when is_boolean(Value) ->
  {Value, Acc};
validate_compile(_Meta, file, nil, Acc) ->
  {nil, Acc};
validate_compile(Meta, Key, Value, Acc) ->
  case is_valid(Key, Value) of
    true ->
      {Value, Acc};
    false ->
      Var = {Key, Meta, ?MODULE},
      Call = {{'.', Meta, [?MODULE, validate_runtime]}, Meta, [Key, Value]},
      {Var, [{'=', Meta, [Var, Call]} | Acc]}
  end.

is_valid(line, Line) -> is_integer(Line);
is_valid(file, File) -> is_binary(File);
is_valid(context, Context) -> is_atom(Context) andalso (Context /= nil);
is_valid(generated, Generated) -> is_boolean(Generated);
is_valid(unquote, Unquote) -> is_boolean(Unquote).
