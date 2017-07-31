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

% Char tokens

% We tokenize char literals (?a) as {char, _, CharInt} instead of {number, _,
% CharInt}. This is exactly what Erlang does with Erlang char literals
% ($a). This means we'll have to adjust the error message for char literals in
% elixir_errors.erl as by default {char, _, _} tokens are "hijacked" by Erlang
% and printed with Erlang syntax ($a) in the parser's error messages.

tokenize([$?, $\\, H | T], Line, Column, Scope, Tokens) ->
  Char = elixir_interpolation:unescape_map(H),
  tokenize(T, Line, Column + 3, Scope, [{char, {Line, Column, Column + 3}, Char} | Tokens]);

tokenize([$?, Char | T], Line, Column, Scope, Tokens) ->
  case handle_char(Char) of
    {Escape, Name} ->
      Msg = io_lib:format("found ? followed by codepoint 0x~.16B (~ts), please use ~ts instead",
                          [Char, Name, Escape]),
      elixir_errors:warn(Line, Scope#elixir_tokenizer.file, Msg);
    false ->
      ok
  end,
  tokenize(T, Line, Column + 2, Scope, [{char, {Line, Column, Column + 2}, Char} | Tokens]);

% Heredocs

tokenize("\"\"\"" ++ T, Line, Column, Scope, Tokens) ->
  handle_heredocs(T, Line, Column, $", Scope, Tokens);

tokenize("'''" ++ T, Line, Column, Scope, Tokens) ->
  handle_heredocs(T, Line, Column, $', Scope, Tokens);

% Strings

tokenize([$" | T], Line, Column, Scope, Tokens) ->
  handle_strings(T, Line, Column + 1, $", Scope, Tokens);
tokenize([$' | T], Line, Column, Scope, Tokens) ->
  handle_strings(T, Line, Column + 1, $', Scope, Tokens);

% Operator atoms

tokenize("...:" ++ Rest, Line, Column, Scope, Tokens) when ?is_space(hd(Rest)) ->
  tokenize(Rest, Line, Column + 4, Scope, [{kw_identifier, {Line, Column, Column + 4}, '...'} | Tokens]);
tokenize("<<>>:" ++ Rest, Line, Column, Scope, Tokens) when ?is_space(hd(Rest)) ->
  tokenize(Rest, Line, Column + 5, Scope, [{kw_identifier, {Line, Column, Column + 5}, '<<>>'} | Tokens]);
tokenize("%{}:" ++ Rest, Line, Column, Scope, Tokens) when ?is_space(hd(Rest)) ->
  tokenize(Rest, Line, Column + 4, Scope, [{kw_identifier, {Line, Column, Column + 4}, '%{}'} | Tokens]);
tokenize("%:" ++ Rest, Line, Column, Scope, Tokens) when ?is_space(hd(Rest)) ->
  tokenize(Rest, Line, Column + 2, Scope, [{kw_identifier, {Line, Column, Column + 2}, '%'} | Tokens]);
tokenize("{}:" ++ Rest, Line, Column, Scope, Tokens) when ?is_space(hd(Rest)) ->
  tokenize(Rest, Line, Column + 3, Scope, [{kw_identifier, {Line, Column, Column + 3}, '{}'} | Tokens]);

tokenize(":..." ++ Rest, Line, Column, Scope, Tokens) ->
  tokenize(Rest, Line, Column + 4, Scope, [{atom, {Line, Column, Column + 4}, '...'} | Tokens]);
tokenize(":<<>>" ++ Rest, Line, Column, Scope, Tokens) ->
  tokenize(Rest, Line, Column + 5, Scope, [{atom, {Line, Column, Column + 5}, '<<>>'} | Tokens]);
tokenize(":%{}" ++ Rest, Line, Column, Scope, Tokens) ->
  tokenize(Rest, Line, Column + 4, Scope, [{atom, {Line, Column, Column + 4}, '%{}'} | Tokens]);
tokenize(":%" ++ Rest, Line, Column, Scope, Tokens) ->
  tokenize(Rest, Line, Column + 2, Scope, [{atom, {Line, Column, Column + 2}, '%'} | Tokens]);
tokenize(":{}" ++ Rest, Line, Column, Scope, Tokens) ->
  tokenize(Rest, Line, Column + 3, Scope, [{atom, {Line, Column, Column + 3}, '{}'} | Tokens]);

% ## Three Token Operators
tokenize([$:, T1, T2, T3 | Rest], Line, Column, Scope, Tokens) when
    ?unary_op3(T1, T2, T3); ?comp_op3(T1, T2, T3); ?and_op3(T1, T2, T3); ?or_op3(T1, T2, T3);
    ?arrow_op3(T1, T2, T3); ?three_op(T1, T2, T3) ->
  tokenize(Rest, Line, Column + 4, Scope, [{atom, {Line, Column, Column + 4}, list_to_atom([T1, T2, T3])} | Tokens]);

% ## Two Token Operators
tokenize([$:, T1, T2 | Rest], Line, Column, Scope, Tokens) when
    ?comp_op2(T1, T2); ?rel_op2(T1, T2); ?and_op(T1, T2); ?or_op(T1, T2);
    ?arrow_op(T1, T2); ?in_match_op(T1, T2); ?two_op(T1, T2); ?stab_op(T1, T2);
    ?type_op(T1, T2) ->
  tokenize(Rest, Line, Column + 3, Scope, [{atom, {Line, Column, Column + 3}, list_to_atom([T1, T2])} | Tokens]);

% ## Single Token Operators
tokenize([$:, T | Rest], Line, Column, Scope, Tokens) when
    ?at_op(T); ?unary_op(T); ?capture_op(T); ?dual_op(T); ?mult_op(T);
    ?rel_op(T); ?match_op(T); ?pipe_op(T); T == $. ->
  tokenize(Rest, Line, Column + 2, Scope, [{atom, {Line, Column, Column + 2}, list_to_atom([T])} | Tokens]);

% Stand-alone tokens

tokenize("..." ++ Rest, Line, Column, Scope, Tokens) ->
  Token = check_call_identifier(Line, Column, 3, '...', Rest),
  tokenize(Rest, Line, Column + 3, Scope, [Token | Tokens]);

tokenize("=>" ++ Rest, Line, Column, Scope, Tokens) ->
  tokenize(Rest, Line, Column + 2, Scope, add_token_with_nl({assoc_op, {Line, Column, Column + 2}, '=>'}, Tokens));

% ## Three token operators
tokenize([T1, T2, T3 | Rest], Line, Column, Scope, Tokens) when ?unary_op3(T1, T2, T3) ->
  handle_unary_op(Rest, Line, Column, unary_op, 3, list_to_atom([T1, T2, T3]), Scope, Tokens);

tokenize([T1, T2, T3 | Rest], Line, Column, Scope, Tokens) when ?comp_op3(T1, T2, T3) ->
  handle_op(Rest, Line, Column, comp_op, 3, list_to_atom([T1, T2, T3]), Scope, Tokens);

tokenize([T1, T2, T3 | Rest], Line, Column, Scope, Tokens) when ?and_op3(T1, T2, T3) ->
  handle_op(Rest, Line, Column, and_op, 3, list_to_atom([T1, T2, T3]), Scope, Tokens);

tokenize([T1, T2, T3 | Rest], Line, Column, Scope, Tokens) when ?or_op3(T1, T2, T3) ->
  handle_op(Rest, Line, Column, or_op, 3, list_to_atom([T1, T2, T3]), Scope, Tokens);

tokenize([T1, T2, T3 | Rest], Line, Column, Scope, Tokens) when ?three_op(T1, T2, T3) ->
  handle_op(Rest, Line, Column, three_op, 3, list_to_atom([T1, T2, T3]), Scope, Tokens);

tokenize([T1, T2, T3 | Rest], Line, Column, Scope, Tokens) when ?arrow_op3(T1, T2, T3) ->
  handle_op(Rest, Line, Column, arrow_op, 3, list_to_atom([T1, T2, T3]), Scope, Tokens);

% ## Containers + punctuation tokens
tokenize([T, T | Rest], Line, Column, Scope, Tokens) when T == $<; T == $> ->
  Token = {list_to_atom([T, T]), {Line, Column, Column + 2}},
  handle_terminator(Rest, Line, Column + 2, Scope, Token, Tokens);

tokenize([T | Rest], Line, Column, Scope, Tokens) when T == $(;
    T == ${; T == $}; T == $[; T == $]; T == $); T == $, ->
  Token = {list_to_atom([T]), {Line, Column, Column + 1}},
  handle_terminator(Rest, Line, Column + 1, Scope, Token, Tokens);

% ## Two Token Operators
tokenize([T1, T2 | Rest], Line, Column, Scope, Tokens) when ?two_op(T1, T2) ->
  handle_op(Rest, Line, Column, two_op, 2, list_to_atom([T1, T2]), Scope, Tokens);

tokenize([T1, T2 | Rest], Line, Column, Scope, Tokens) when ?arrow_op(T1, T2) ->
  handle_op(Rest, Line, Column, arrow_op, 2, list_to_atom([T1, T2]), Scope, Tokens);

tokenize([T1, T2 | Rest], Line, Column, Scope, Tokens) when ?comp_op2(T1, T2) ->
  handle_op(Rest, Line, Column, comp_op, 2, list_to_atom([T1, T2]), Scope, Tokens);

tokenize([T1, T2 | Rest], Line, Column, Scope, Tokens) when ?rel_op2(T1, T2) ->
  handle_op(Rest, Line, Column, rel_op, 2, list_to_atom([T1, T2]), Scope, Tokens);

tokenize([T1, T2 | Rest], Line, Column, Scope, Tokens) when ?and_op(T1, T2) ->
  handle_op(Rest, Line, Column, and_op, 2, list_to_atom([T1, T2]), Scope, Tokens);

tokenize([T1, T2 | Rest], Line, Column, Scope, Tokens) when ?or_op(T1, T2) ->
  handle_op(Rest, Line, Column, or_op, 2, list_to_atom([T1, T2]), Scope, Tokens);

tokenize([T1, T2 | Rest], Line, Column, Scope, Tokens) when ?in_match_op(T1, T2) ->
  handle_op(Rest, Line, Column, in_match_op, 2, list_to_atom([T1, T2]), Scope, Tokens);

tokenize([T1, T2 | Rest], Line, Column, Scope, Tokens) when ?type_op(T1, T2) ->
  handle_op(Rest, Line, Column, type_op, 2, list_to_atom([T1, T2]), Scope, Tokens);

tokenize([T1, T2 | Rest], Line, Column, Scope, Tokens) when ?stab_op(T1, T2) ->
  handle_op(Rest, Line, Column, stab_op, 2, list_to_atom([T1, T2]), Scope, Tokens);

% ## Single Token Operators

tokenize([T | Rest], Line, Column, Scope, Tokens) when ?at_op(T) ->
  handle_unary_op(Rest, Line, Column, at_op, 1, list_to_atom([T]), Scope, Tokens);

tokenize([T | Rest], Line, Column, Scope, Tokens) when ?capture_op(T) ->
  handle_unary_op(Rest, Line, Column, capture_op, 1, list_to_atom([T]), Scope, Tokens);

tokenize([T | Rest], Line, Column, Scope, Tokens) when ?unary_op(T) ->
  handle_unary_op(Rest, Line, Column, unary_op, 1, list_to_atom([T]), Scope, Tokens);

tokenize([T | Rest], Line, Column, Scope, Tokens) when ?rel_op(T) ->
  handle_op(Rest, Line, Column, rel_op, 1, list_to_atom([T]), Scope, Tokens);

tokenize([T | Rest], Line, Column, Scope, Tokens) when ?dual_op(T) ->
  handle_unary_op(Rest, Line, Column, dual_op, 1, list_to_atom([T]), Scope, Tokens);

tokenize([T | Rest], Line, Column, Scope, Tokens) when ?mult_op(T) ->
  handle_op(Rest, Line, Column, mult_op, 1, list_to_atom([T]), Scope, Tokens);

tokenize([T | Rest], Line, Column, Scope, Tokens) when ?match_op(T) ->
  handle_op(Rest, Line, Column, match_op, 1, list_to_atom([T]), Scope, Tokens);

tokenize([T | Rest], Line, Column, Scope, Tokens) when ?pipe_op(T) ->
  handle_op(Rest, Line, Column, pipe_op, 1, list_to_atom([T]), Scope, Tokens);

% Non-operator Atoms

tokenize([$:, H | T] = Original, Line, Column, Scope, Tokens) when ?is_quote(H) ->
  case elixir_interpolation:extract(Line, Column + 2, Scope, true, T, H) of
    {NewLine, NewColumn, Parts, Rest} ->
      Unescaped = unescape_tokens(Parts),
      Key = case Scope#elixir_tokenizer.existing_atoms_only of
        true  -> atom_safe;
        false -> atom_unsafe
      end,
      tokenize(Rest, NewLine, NewColumn, Scope, [{Key, {Line, Column, NewColumn}, Unescaped} | Tokens]);
    {error, Reason} ->
      interpolation_error(Reason, Original, Tokens, " (for atom starting at line ~B)", [Line])
  end;

tokenize([$: | String] = Original, Line, Column, Scope, Tokens) ->
  case tokenize_identifier(String, Line, Scope) of
    {_Kind, Atom, Rest, Length, _Ascii, _Special} ->
      tokenize(Rest, Line, Column + 1 + Length, Scope, [{atom, {Line, Column, Column + 1 + Length}, Atom} | Tokens]);
    empty ->
      unexpected_token(Original, Line, Column, Tokens);
    {error, Reason} ->
      {error, Reason, Original, Tokens}
  end;

% Integers and floats

tokenize([H | T], Line, Column, Scope, Tokens) when ?is_digit(H) ->
  case tokenize_number(T, [H], 1, false) of
    {error, Reason, Number} ->
      {error, {Line, Reason, Number}, T, Tokens};
    {Rest, Number, Length} when is_integer(Number) ->
      tokenize(Rest, Line, Column + Length, Scope, [{decimal, {Line, Column, Column + Length}, Number} | Tokens]);
    {Rest, Number, Length} ->
      tokenize(Rest, Line, Column + Length, Scope, [{float, {Line, Column, Column + Length}, Number} | Tokens])
  end;

% Spaces

tokenize([T | Rest], Line, Column, Scope, Tokens) when ?is_horizontal_space(T) ->
  {Remaining, Stripped} = strip_horizontal_space(Rest),
  handle_space_sensitive_tokens(Remaining, Line, Column + 1 + Stripped, Scope, Tokens);

% End of line

tokenize(";" ++ Rest, Line, Column, Scope, []) ->
  tokenize(Rest, Line, Column + 1, Scope, [{';', {Line, Column, Column + 1}}]);

tokenize(";" ++ Rest, Line, Column, Scope, [Top | _] = Tokens) when element(1, Top) /= ';' ->
  tokenize(Rest, Line, Column + 1, Scope, [{';', {Line, Column, Column + 1}} | Tokens]);

tokenize("\\" = Original, Line, _Column, _Scope, Tokens) ->
  {error, {Line, "invalid escape \\ at end of file", []}, Original, Tokens};

tokenize("\\\n" = Original, Line, _Column, _Scope, Tokens) ->
  {error, {Line, "invalid escape \\ at end of file", []}, Original, Tokens};

tokenize("\\\r\n" = Original, Line, _Column, _Scope, Tokens) ->
  {error, {Line, "invalid escape \\ at end of file", []}, Original, Tokens};

tokenize("\\\n" ++ Rest, Line, _Column, Scope, Tokens) ->
  tokenize(Rest, Line + 1, 1, Scope, Tokens);

tokenize("\\\r\n" ++ Rest, Line, _Column, Scope, Tokens) ->
  tokenize(Rest, Line + 1, 1, Scope, Tokens);

tokenize("\n" ++ Rest, Line, Column, Scope, Tokens) ->
  tokenize(Rest, Line + 1, 1, Scope, eol(Line, Column, Tokens));

tokenize("\r\n" ++ Rest, Line, Column, Scope, Tokens) ->
  tokenize(Rest, Line + 1, 1, Scope, eol(Line, Column, Tokens));

% Others

tokenize([$%, ${ | T], Line, Column, Scope, Tokens) ->
  tokenize([${ | T], Line, Column + 1, Scope, [{'%{}', {Line, Column, Column + 1}} | Tokens]);

tokenize([$% | T], Line, Column, Scope, Tokens) ->
  tokenize(T, Line, Column + 1, Scope, [{'%', {Line, Column, Column + 1}} | Tokens]);

tokenize([$. | T], Line, Column, Scope, Tokens) ->
  {Rest, Counter, Offset, CommentTokens} = strip_dot_space(T, 0, Column + 1, Line, []),
  handle_dot([$. | Rest], Line, Offset - 1, Column, Scope, Tokens, CommentTokens, Counter);

% Identifiers

tokenize(String, Line, Column, Scope, Tokens) ->
  case tokenize_identifier(String, Line, Scope) of
    {Kind, Atom, Rest, Length, Ascii, Special} ->
      HasAt = lists:member($@, Special),

      case Rest of
        [$: | T] when ?is_space(hd(T)) ->
          tokenize(T, Line, Column + Length + 1, Scope, [{kw_identifier, {Line, Column, Column + Length + 1}, Atom} | Tokens]);
        [$: | T] when hd(T) /= $: ->
          AtomName = atom_to_list(Atom) ++ [$:],
          Reason = {Line, "keyword argument must be followed by space after: ", AtomName},
          {error, Reason, String, Tokens};
        _ when HasAt ->
          Reason = {Line, invalid_character_error(Kind, $@), atom_to_list(Atom)},
          {error, Reason, String, Tokens};
        _ when Kind == alias ->
          tokenize_alias(Rest, Line, Column, Atom, Length, Ascii, Special, Scope, Tokens);
        _ when Kind == identifier ->
          tokenize_other(Rest, Line, Column, Atom, Length, Scope, Tokens);
        _ ->
          unexpected_token(String, Line, Column, Tokens)
      end;
    empty ->
      unexpected_token(String, Line, Column, Tokens);
    {error, Reason} ->
      {error, Reason, String, Tokens}
  end.

