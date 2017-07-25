% Handle string and string-like interpolations.
-module(elixir_interpolation).
-export([extract/6, unescape_chars/1, unescape_chars/2,
unescape_tokens/1, unescape_tokens/2, unescape_map/1]).
-include("elixir.hrl").
-define(is_hex(S), ((S >= $0 andalso S =< $9) orelse
                    (S >= $A andalso S =< $F) orelse
                    (S >= $a andalso S =< $f))).

%% Extract string interpolations

extract(Line, Column, Raw, Interpol, String, Last) ->
  %% Ignore whatever is in the scope and enable terminator checking.
  Scope = Raw#elixir_tokenizer{terminators=[], check_terminators=true},
  extract(Line, Column, Scope, Interpol, String, [], [], Last).

%% Terminators

extract(Line, Column, _Scope, _Interpol, [], Buffer, Output, []) ->
  finish_extraction(Line, Column, Buffer, Output, []);

extract(Line, _Column, _Scope, _Interpol, [], _Buffer, _Output, Last) ->
  {error, {string, Line, io_lib:format("missing terminator: ~ts", [[Last]]), []}};

extract(Line, Column, _Scope, _Interpol, [Last | Remaining], Buffer, Output, Last) ->
  finish_extraction(Line, Column + 1, Buffer, Output, Remaining);

%% Going through the string

extract(Line, _Column, Scope, true, [$\\, $\n | Rest], Buffer, Output, Last) ->
  extract(Line+1, 1, Scope, true, Rest, Buffer, Output, Last);

extract(Line, _Column, Scope, true, [$\\, $\r, $\n | Rest], Buffer, Output, Last) ->
  extract(Line+1, 1, Scope, true, Rest, Buffer, Output, Last);

extract(Line, _Column, Scope, Interpol, [$\n | Rest], Buffer, Output, Last) ->
  extract(Line+1, 1, Scope, Interpol, Rest, [$\n | Buffer], Output, Last);

extract(Line, Column, Scope, Interpol, [$\\, Last | Rest], Buffer, Output, Last) ->
  extract(Line, Column+2, Scope, Interpol, Rest, [Last | Buffer], Output, Last);

extract(Line, Column, Scope, true, [$\\, $#, ${ | Rest], Buffer, Output, Last) ->
  extract(Line, Column+1, Scope, true, Rest, [${, $# | Buffer], Output, Last);

extract(Line, Column, Scope, true, [$#, ${ | Rest], Buffer, Output, Last) ->
  Output1 = build_string(Line, Buffer, Output),
  case elixir_tokenizer:tokenize(Rest, Line, Column + 2, Scope) of
    {error, {{EndLine, _, EndColumn}, _, "}"}, [$} | NewRest], Tokens} ->
      Output2 = build_interpol(Line, Column, EndColumn, Tokens, Output1),
      extract(EndLine, EndColumn, Scope, true, NewRest, [], Output2, Last);
    {error, Reason, _, _} ->
      {error, Reason};
    {ok, _EndLine, _EndColumn, _} ->
      {error, {string, Line, "missing interpolation terminator:}", []}}
  end;

extract(Line, Column, Scope, Interpol, [$\\, Char | Rest], Buffer, Output, Last) ->
  extract(Line, Column+2, Scope, Interpol, Rest, [Char, $\\ | Buffer], Output, Last);

%% Catch all clause

extract(Line, Column, Scope, Interpol, [Char | Rest], Buffer, Output, Last) ->
  extract(Line, Column + 1, Scope, Interpol, Rest, [Char | Buffer], Output, Last).

%% Unescape a series of tokens as returned by extract.

unescape_tokens(Tokens) ->
  unescape_tokens(Tokens, fun unescape_map/1).

unescape_tokens(Tokens, Map) ->
  [unescape_token(Token, Map) || Token <- Tokens].

unescape_token(Token, Map) when is_binary(Token) -> unescape_chars(Token, Map);
unescape_token(Other, _Map) -> Other.

% Unescape chars. For instance, "\" "n" (two chars) needs to be converted to "\n" (one char).

unescape_chars(String) ->
  unescape_chars(String, fun unescape_map/1).

unescape_chars(String, Map) ->
  unescape_chars(String, Map, <<>>).

unescape_chars(<<$\\, $x, Rest/binary>>, Map, Acc) ->
  case Map($x) of
    true  -> unescape_hex(Rest, Map, Acc);
    false -> unescape_chars(Rest, Map, <<Acc/binary, $\\, $x>>)
  end;

unescape_chars(<<$\\, $u, Rest/binary>>, Map, Acc) ->
  case Map($u) of
    true  -> unescape_unicode(Rest, Map, Acc);
    false -> unescape_chars(Rest, Map, <<Acc/binary, $\\, $u>>)
  end;

  unescape_chars(<<$\\, Escaped, Rest/binary>>, Map, Acc) ->
    case Map(Escaped) of
      false -> unescape_chars(Rest, Map, <<Acc/binary, $\\, Escaped>>);
      Other -> unescape_chars(Rest, Map, <<Acc/binary, Other>>)
    end;

unescape_chars(<<Char, Rest/binary>>, Map, Acc) ->
  unescape_chars(Rest, Map, <<Acc/binary, Char>>);

unescape_chars(<<>>, _Map, Acc) -> Acc.

% Unescape Helpers

unescape_hex(<<A, B, Rest/binary>>, Map, Acc) when ?is_hex(A), ?is_hex(B) ->
  Bytes = list_to_integer([A, B], 16),
  unescape_chars(Rest, Map, <<Acc/binary, Bytes>>);

%% TODO: Remove deprecated sequences in v2.0

unescape_hex(<<A, Rest/binary>>, Map, Acc) when ?is_hex(A) ->
  io:format(standard_error, "warning: \\xH inside strings/sigils/chars is deprecated, please use \\xHH (byte) or \\uHHHH (codepoint) instead~n", []),
  append_codepoint(Rest, Map, [A], Acc, 16);

unescape_hex(<<${, A, $}, Rest/binary>>, Map, Acc) when ?is_hex(A) ->
  io:format(standard_error, "warning: \\x{H*} inside strings/sigils/chars is deprecated, please use \\xHH (byte) or \\uHHHH (codepoint) instead~n", []),
  append_codepoint(Rest, Map, [A], Acc, 16);
  end;
