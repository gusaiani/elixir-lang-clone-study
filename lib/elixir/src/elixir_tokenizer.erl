-module(elixir_tokenizer).
-include("elixir.hrl").
-export([tokenize/1, tokenize/3, tokenize/4, invalid_do_error/1]).
-import(elixir_interpolation, [unescape_tokens/1]).

%% Numbers
-define(is_hex(S), (?is_digit(S) orelse (S >= $A andalso S =< $F) orelse (S >= $a andalso S =< $f))).
-define(is_bin(S), (S >= $0 andalso S =< $1)).
-define(is_octal(S), (S >= $0 andalso S =< $7)).

%% Digits and letters
-define(is_digit(S), (S >= $0 andalso S =< $9)).
-define(is_upcase(S), (S >= $A andalso S =< $Z)).
-define(is_downcase(S), (S >= $a andalso S =< $z)).

%% Others
-define(is_quote(S), (S == $" orelse S == $')).
-define(is_sigil(S), ((S == $/) orelse (S == $<) orelse (S == $") orelse (S == $') orelse
                      (S == $[) orelse (S == $() orelse (S == ${) orelse (S == $|))).

%% Spaces
-define(is_horizontal_space(S), ((S == $\s) orelse (S == $\t))).
-define(is_vertical_space(S), ((S == $\r) orelse (S == $\n))).
-define(is_space(S), (?is_horizontal_space(S) orelse ?is_vertical_space(S))).

%% Operators
-define(at_op(T),
  T == $@).

-define(capture_op(T),
  T == $&).

-define(unary_op(T),
  T == $!;
  T == $^).

-define(unary_op3(T1, T2, T3),
  T1 == $~, T2 == $~, T3 == $~).

-define(two_op(T1, T2),
  T1 == $+, T2 == $+;
  T1 == $-, T2 == $-;
  T1 == $<, T2 == $>;
  T1 == $., T2 == $.).

-define(three_op(T1, T2, T3),
  T1 == $^, T2 == $^, T3 == $^).

-define(mult_op(T),
  T == $* orelse T == $/).

-define(dual_op(T),
  T == $+ orelse T == $-).

-define(arrow_op3(T1, T2, T3),
  T1 == $<, T2 == $<, T3 == $<;
  T1 == $>, T2 == $>, T3 == $>;
  T1 == $~, T2 == $>, T3 == $>;
  T1 == $<, T2 == $<, T3 == $~;
  T1 == $<, T2 == $~, T3 == $>;
  T1 == $<, T2 == $|, T3 == $>).

-define(arrow_op(T1, T2),
  T1 == $|, T2 == $>;
  T1 == $~, T2 == $>;
  T1 == $<, T2 == $~).

-define(rel_op(T),
  T == $<;
  T == $>).

-define(rel_op2(T1, T2),
  T1 == $<, T2 == $=;
  T1 == $>, T2 == $=).

-define(comp_op2(T1, T2),
  T1 == $=, T2 == $=;
  T1 == $=, T2 == $~;
  T1 == $!, T2 == $=).

-define(comp_op3(T1, T2, T3),
  T1 == $=, T2 == $=, T3 == $=;
  T1 == $!, T2 == $=, T3 == $=).

-define(and_op(T1, T2),
  T1 == $&, T2 == $&).

-define(or_op(T1, T2),
  T1 == $|, T2 == $|).

-define(and_op3(T1, T2, T3),
  T1 == $&, T2 == $&, T3 == $&).

-define(or_op3(T1, T2, T3),
  T1 == $|, T2 == $|, T3 == $|).

-define(match_op(T),
  T == $=).

-define(in_match_op(T1, T2),
  T1 == $<, T2 == $-;
  T1 == $\\, T2 == $\\).

-define(stab_op(T1, T2),
  T1 == $-, T2 == $>).

-define(type_op(T1, T2),
  T1 == $:, T2 == $:).

-define(pipe_op(T),
  T == $|).

tokenize(String, Line, Column, #elixir_tokenizer{} = Scope) ->
  tokenize(String, Line, Column, Scope, []);

tokenize(String, Line, Column, Opts) ->
  File = case lists:keyfind(file, 1, Opts) of
    {file, V1} -> V1;
    false -> <<"nofile">>
  end,

  ExistingAtomsOnly = case lists:keyfind(existing_atoms_only, 1, Opts) of
    {existing_atoms_only, ExistingAtomsOnlyBool} when
      is_boolean(ExistingAtomsOnlyBool) -> ExistingAtomsOnlyBool;
    _ -> false
  end,

  CheckTerminators = case lists:keyfind(check_terminators, 1, Opts) of
    {check_terminators, CheckTerminatorsBool} when
      is_boolean(CheckTerminatorsBool) -> CheckTerminatorsBool;
    _ -> true
  end,

  PreserveComments = case lists:keyfind(preserve_comments, 1, Opts) of
    {preserve_comments, PreserveCommentsBool} when
      is_boolean(PreserveCommentsBool) -> PreserveCommentsBool;
    _ -> false
  end,

  tokenize(String, Line, Column, #elixir_tokenizer{
    file=File,
    existing_atoms_only=ExistingAtomsOnly,
    check_terminators=CheckTerminators,
    preserve_comments=PreserveComments,
    identifier_tokenizer=elixir_config:get(identifier_tokenizer)
  }).

tokenize(String, Line, Opts) ->
  tokenize(String, Line, 1, Opts).

tokenize([], Line, Column, #elixir_tokenizer{terminators=[]}, Tokens) ->
  {ok, Line, Column, lists:reverse(Tokens)};

tokenize([], EndLine, _Column, #elixir_tokenizer{terminators=[{Start, {StartLine, _, _}} | _]}, Tokens) ->
  End     = terminator(Start),
  Message = io_lib:format("missing terminator: ~ts (for \"~ts\" starting at line ~B)", [End, Start, StartLine]),
  {error, {EndLine, Message, []}, [], Tokens};

% VC merge conflict

tokenize(("<<<<<<<" ++ _) = Original, Line, 1, _Scope, Tokens) ->
  FirstLine = lists:takewhile(fun(C) -> C =/= $\n andalso C =/= $\r end, Original),
  {error, {Line, "found an unexpected version control marker, please resolve the conflicts: ", FirstLine}, Original, Tokens};

% Base integers

tokenize([$0, $x, H | T], Line, Column, Scope, Tokens) when ?is_hex(H) ->
  {Rest, Number, Length} = tokenize_hex(T, [H], 1),
  tokenize(Rest, Line, Column + 2 + Length, Scope, [{hexadecimal, {Line, Column, Column + 2 + Length}, Number} | Tokens]);

tokenize([$0, $b, H | T], Line, Column, Scope, Tokens) when ?is_bin(H) ->
  {Rest, Number, Length} = tokenize_bin(T, [H], 1),
  tokenize(Rest, Line, Column + 2 + Length, Scope, [{binary, {Line, Column, Column + 2 + Length}, Number} | Tokens]);

tokenize([$0, $o, H | T], Line, Column, Scope, Tokens) when ?is_octal(H) ->
  {Rest, Number, Length} = tokenize_octal(T, [H], 1),
  tokenize(Rest, Line, Column + 2 + Length, Scope, [{octal, {Line, Column, Column + 2 + Length}, Number} | Tokens]);

% Comments

tokenize([$# | String], Line, Column, Scope, Tokens) ->
  {Rest, Comment, Length} = tokenize_comment(String, [$#], 1),
  case Scope#elixir_tokenizer.preserve_comments of
    true  ->
      CommentToken = {comment, {Line, Column, Column + Length}, Comment},
      tokenize(Rest, Line, Column + Length, Scope, [CommentToken | Tokens]);
    false ->
      tokenize(Rest, Line, Column, Scope, Tokens)
  end;

% Sigils

tokenize([$~, S, H, H, H | T] = Original, Line, Column, Scope, Tokens) when ?is_quote(H), ?is_upcase(S) orelse ?is_downcase(S) ->
  case extract_heredoc_with_interpolation(Line, Column, Scope, ?is_downcase(S), T, H) of
    {ok, NewLine, NewColumn, Parts, Rest} ->
      {Final, Modifiers} = collect_modifiers(Rest, []),
      tokenize(Final, NewLine, NewColumn, Scope, [{sigil, {Line, Column, NewColumn}, S, Parts, Modifiers, [H, H, H]} | Tokens]);
    {error, Reason} ->
      {error, Reason, Original, Tokens}
  end;

tokenize([$~, S, H | T] = Original, Line, Column, Scope, Tokens) when ?is_sigil(H), ?is_upcase(S) orelse ?is_downcase(S) ->
  case elixir_interpolation:extract(Line, Column + 3, Scope, ?is_downcase(S), T, sigil_terminator(H)) of
    {NewLine, NewColumn, Parts, Rest} ->
      {Final, Modifiers} = collect_modifiers(Rest, []),
      tokenize(Final, NewLine, NewColumn, Scope, [{sigil, {Line, Column, NewColumn}, S, Parts, Modifiers, [H]} | Tokens]);
    {error, Reason} ->
      Sigil = [$~, S, H],
      interpolation_error(Reason, Original, Tokens, " (for sigil ~ts starting at line ~B)", [Sigil, Line])
  end;

tokenize([$~, S, H | _] = Original, Line, Column, _Scope, Tokens) when ?is_upcase(S) orelse ?is_downcase(S) ->
  MessageString =
    "\"~ts\" (column ~p, codepoint U+~4.16.0B). The available delimiters are: "
    "//, ||, \"\", '', (), [], {}, <>",
  Message = io_lib:format(MessageString, [[H], Column + 2, H]),
  {error, {Line, "invalid sigil delimiter: ", Message}, Original, Tokens};
