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
          <<"invalid runtime value for option :", (erlang:atom_to_binary(Key, utf8))/binary,
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

do_linify(LinifyMeta, {Left, Meta, Receiver}, {Receiver, Counter} = Var)
    when is_atom(Left), is_list(Meta), Left /= '_' ->
  do_tuple_linify(LinifyMeta, keynew(counter, Meta, Counter), Left, Receiver, Var);

do_linify(LinifyMeta, {Lexical, Meta, [_ | _] = Args}, {_, Counter} = Var)
    when ?lexical(Lexical); Lexical == '__aliases__' ->
  do_tuple_linify(LinifyMeta, keynew(counter, Meta, Counter), Lexical, Args, Var);

do_linify(LinifyMeta, {Left, Meta, Right}, Var) when is_list(Meta) ->
  do_tuple_linify(LinifyMeta, Meta, Left, Right, Var);

do_linify(LinifyMeta, {Left, Right}, Var) ->
  {do_linify(LinifyMeta, Left, Var), do_linify(LinifyMeta, Right, Var)};

do_linify(LinifyMeta, List, Var) when is_list(List) ->
  [do_linify(LinifyMeta, X, Var) || X <- List];

do_linify(_, Else, _) -> Else.

do_tuple_linify(LinifyMeta, Meta, Left, Right, Var) ->
  {do_linify(LinifyMeta, Left, Var), LinifyMeta(Meta), do_linify(LinifyMeta, Right, Var)}.

linify_meta(0, line) -> fun(Meta) -> Meta end;
linify_meta(Line, line) -> fun(Meta) -> keynew(line, Meta, Line) end;
linify_meta(Line, keep) ->
  fun(Meta) ->
    case lists:keytake(keep, 1, Meta) of
      {value, {keep, {_, Int}}, MetaNoFile} ->
        [{line, Int} | keydelete(line, MetaNoFile)];
      _ ->
        keynew(line, Meta, Line)
    end
  end.

%% Some expressions cannot be unquoted at compilation time.
%% This function is responsible for doing runtime unquoting.
dot(Meta, Left, Right, Args, Context) ->
  annotate(dot(Meta, Left, Right, Args), Context).

dot(Meta, Left, {'__aliases__', _, Args}, nil) ->
  {'__aliases__', Meta, [Left | Args]};

dot(Meta, Left, Right, nil) when is_atom(Right) ->
  case atom_to_list(Right) of
    "Elixir." ++ _ ->
      {'__aliases__', Meta, [Left, Right]};
    _ ->
      {{'.', Meta, [Left, Right]}, [{no_parens, true} | Meta], []}
  end;

dot(Meta, Left, {Right, _, Context}, nil) when is_atom(Right), is_atom(Context) ->
  {{'.', Meta, [Left, Right]}, [{no_parens, true} | Meta], []};

dot(Meta, Left, {Right, _, Args}, nil) when is_atom(Right) ->
  {{'.', Meta, [Left, Right]}, Meta, Args};

dot(_Meta, _Left, Right, nil) ->
  argument_error(<<"expected unquote after dot to return an atom, an alias or a quoted call, got: ",
                   ('Elixir.Macro':to_string(Right))/binary>>);

dot(Meta, Left, Right, Args) when is_atom(Right) ->
  {{'.', Meta, [Left, Right]}, Meta, Args};

dot(Meta, Left, {Right, _, Context}, Args) when is_atom(Right), is_atom(Context) ->
  {{'.', Meta, [Left, Right]}, Meta, Args};

dot(_Meta, _Left, Right, _Args) ->
  argument_error(<<"expected unquote after dot with args to return an atom or a quoted call, got: ",
                   ('Elixir.Macro':to_string(Right))/binary>>).

list(Left, Right) when is_list(Right) ->
  validate_list(Left),
  Left ++ Right.

tail_list(Left, Right, Tail) when is_list(Right), is_list(Tail) ->
  validate_list(Left),
  Tail ++ Left ++ Right;

tail_list(Left, Right, Tail) when is_list(Left) ->
  validate_list(Left),
  [H | T] = lists:reverse(Tail ++ Left),
  lists:reverse([{'|', [], [H, Right]} | T]).

validate_list(List) when is_list(List) ->
  ok;
validate_list(List) when not is_list(List) ->
  argument_error(<<"expected a list with quoted expressions in unquote_splicing/1, got: ",
                   ('Elixir.Kernel':inspect(List))/binary>>).

argument_error(Message) ->
  error('Elixir.ArgumentError':exception([{message, Message}])).

%% Annotates the AST with context and other info.
%%
%% Note we need to delete the counter because linify
%% adds the counter recursively, even inside quoted
%% expressions, so we need to clean up the forms to
%% allow them to get a new counter on the next expansion.

annotate({Def, Meta, [{H, M, A} | T]}, Context) when ?defs(Def) ->
  {Def, Meta, [{H, keystore(context, M, Context), A} | T]};
annotate({{'.', _, [_, Def]} = Target, Meta, [{H, M, A} | T]}, Context) when ?defs(Def) ->
  {Target, Meta, [{H, keystore(context, M, Context), A} | T]};

annotate({Lexical, Meta, [_ | _] = Args}, Context) when ?lexical(Lexical) ->
  NewMeta = keystore(context, keydelete(counter, Meta), Context),
  {Lexical, NewMeta, Args};
annotate(Tree, _Context) -> Tree.

has_unquotes({unquote, _, [_]}) -> true;
has_unquotes({unquote_splicing, _, [_]}) -> true;
has_unquotes({{'.', _, [_, unquote]}, _, [_]}) -> true;
has_unquotes({Var, _, Ctx}) when is_atom(Var), is_atom(Ctx) -> false;
has_unquotes({Name, _, Args}) when is_list(Args) ->
  has_unquotes(Name) orelse lists:any(fun has_unquotes/1, Args);
has_unquotes({Left, Right}) ->
  has_unquotes(Left) orelse has_unquotes(Right);
has_unquotes(List) when is_list(List) ->
  lists:any(fun has_unquotes/1, List);
has_unquotes(_Other) -> false.

%% Escapes the given expression. It is similar to quote, but
%% lines are kept and hygiene mechanisms are disabled.
escape(Expr, Kind, Unquote) ->
  do_quote(Expr, #elixir_quote{
    line=true,
    file=nil,
    vars_hygiene=false,
    aliases_hygiene=false,
    imports_hygiene=false,
    unquote=Unquote
  }, Kind).

%% fun_to_quoted

fun_to_quoted(Function) ->
  Meta = [],
  {module, Module} = erlang:fun_info(Function, module),
  {name, Name}     = erlang:fun_info(Function, name),
  {arity, Arity}   = erlang:fun_info(Function, arity),
  {'&', Meta, [{'/', Meta, [{{'.', Meta, [Module, Name]}, [{no_parens, true} | Meta], []}, Arity]}]}.

%% Quotes an expression and return its quoted Elixir AST.

quote(_Meta, {unquote_splicing, _, [_]}, _Binding, #elixir_quote{unquote=true}, _, _) ->
  argument_error(<<"unquote_splicing only works inside arguments and block contexts, "
    "wrap it in parens if you want it to work with one-liners">>);

quote(Meta, Expr, Binding, Q, Prelude, E) ->
  Context = Q#elixir_quote.context,

  Vars = [{'{}', [],
    ['=', [], [
      {'{}', [], [K, Meta, Context]},
      V
    ]]
  } || {K, V} <- Binding],

  Quoted = do_quote(Expr, Q, E),

  WithVars = case Vars of
    [] -> Quoted;
    _ -> {'{}', [], ['__block__', [], Vars ++ [Quoted]]}
  end,

  case Prelude of
    [] -> WithVars;
    _ -> {'__block__', [], Prelude ++ [WithVars]}
  end.

%% Actual quoting and helpers

do_quote({quote, Meta, [Arg]}, Q, E) ->
  TArg = do_quote(Arg, Q#elixir_quote{unquote=false}, E),

  NewMeta = case Q of
    #elixir_quote{vars_hygiene=true, context=Context} -> keystore(context, Meta, Context);
    _ -> Meta
  end,

  {'{}', [], [quote, meta(NewMeta, Q), [TArg]]};

do_quote({quote, Meta, [Opts, Arg]}, Q, E) ->
  TOpts = do_quote(Opts, Q, E),
  TArg = do_quote(Arg, Q#elixir_quote{unquote=false}, E),

  NewMeta = case Q of
    #elixir_quote{vars_hygiene=true, context=Context} -> keystore(context, Meta, Context);
    _ -> Meta
  end,

  {'{}', [], [quote, meta(NewMeta, Q), [TOpts, TArg]]};

do_quote({unquote, _Meta, [Expr]}, #elixir_quote{unquote=true}, _) ->
  Expr;

%% Helpers

meta(Meta, Q) ->
  generated(keep(Meta, Q), Q).

generated(Meta, #elixir_quote{generated=true}) -> [{generated, true} | Meta];
generated(Meta, #elixir_quote{generated=false}) -> Meta.

keep(Meta, #elixir_quote{file=nil, line=Line}) ->
  line(Meta, Line);
keep(Meta, #elixir_quote{file=File}) ->
  case lists:keytake(line, 1, Meta) of
    {value, {line, Line}, MetaNoLine} ->
      [{keep, {File, Line}} | MetaNoLine];
    false ->
      [{keep, {File, 0}} | Meta]
  end.

line(Meta, true) ->
  Meta;
line(Meta, false) ->
  keydelete(line, Meta);
line(Meta, Line) ->
  keystore(line, Meta, Line).

keydelete(Key, Meta) ->
  lists:keydelete(Key, 1, Meta).
keystore(_Key, Meta, nil) ->
  Meta;
keystore(Key, Meta, Value) ->
  lists:keystore(Key, 1, Meta, {Key, Value}).

keynew(Key, Meta, Value) ->
  case lists:keymember(Key, 1, Meta) of
    true -> Meta;
    false -> [{Key, Value} | Meta]
  end.
