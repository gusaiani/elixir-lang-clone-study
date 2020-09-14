-module(elixir_quote).
-export([escape/3, linify/3, linify_with_context_counter/3, build/6, quote/6, has_unquotes/1, fun_to_quoted/1]).
-export([dot/5, tail_list/3, list/2, validate_runtime/2]). %% Quote callbacks

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
  {EFile, Acc2} = validate_compile(Meta, file, File, Acc1),
  {EContext, Acc3} = validate_compile(Meta, context, Context, Acc2),
  validate_runtime(unquote, Unquote),
  validate_runtime(generated, Generated),

  Q = #elixir_quote{
    line=ELine,
    file=EFile,
    unquote=Unquote,
    context=EContext,
    generated=Generated
  },

  {Q, Acc3}.

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

validate_runtime(Key, Value) ->
  case is_valid(Key, Value) of
    true ->
      Value;

    false ->
      erlang:error(
        'Elixir.ArgumentError':exception(
          <<"invalid runtime value for option ", (erlang:atom_to_binary(Key, utf8))/binary,
            " in quote, got: ", ('Elixir.Kernel':inspect(Value))/binary>>
        )
      )
  end.

is_valid(line, Line) -> is_integer(Line);
is_valid(file, File) -> is_binary(File);
is_valid(context, Context) -> is_atom(Context) andalso (Context /= nil);
is_valid(generated, Generated) -> is_boolean(Generated);
is_valid(unquote, Unquote) -> is_boolean(Unquote).

%% Apply the line from site call on quoted contents.
%% Receives a Key to look for the default line as argument.
linify(0, _Key, Exprs) ->
  Exprs;
linify(Line, Key, Exprs) when is_integer(Line) ->
  LinifyMeta = linify_meta(Line, Key),
  do_linify(LinifyMeta, Exprs, nil).

%% Same as linify but also considers the context counter.
linify_with_context_counter(Line, Var, Exprs) when is_integer(Line) ->
  LinifyMeta = linify_meta(Line, line),
  do_linify(LinifyMeta, Exprs, Var).

do_linify(LinifyMeta, {quote, Meta, [_ | _] = Args}, {Receiver, Counter} = Var)
    when is_list(Meta) ->
  NewMeta =
    case keyfind(context, Meta) == {context, Receiver} of
      true -> keynew(counter, Meta, Counter);
      false -> Meta
    end,
  do_tuple_linify(LinifyMeta, NewMeta, quote, Args, Var);

do_tuple_linify(LinifyMeta, Meta, Left, Right, Var) ->
  {do_linify(LinifyMeta, Left, Var), LinifyMeta(Meta), do_linify(LinifyMeta, Right, Var)}.

keynew(Key, Meta, Value) ->
  case lists:keymember(Key, 1, Meta) of
    true -> Meta;
    false -> [{Key, Value} | Meta]
  end.
