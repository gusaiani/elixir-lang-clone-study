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
