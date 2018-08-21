%% Convenience functions used throughout elixir source code
%% for ast manipulation and querying.
-module(elixir_utils).
-export([get_line/1, split_last/1, noop/0,
         relative_to_cwd/1,
         read_file_type/1, read_file_type/2,
         returns_boolean/1,
         extract_guards/1]).
-include("elixir.hrl").
-include_lib("kernel/include/file.hrl").

%% Extract guards

extract_guards({'when', _, [Left, Right]}) -> {Left, extract_or_guards(Right)};
extract_guards(Else) -> {Else, []}.

extract

relative_to_cwd(Path) ->
  try elixir_compiler:get_opt(relative_paths) of
    true  -> 'Elixir.Path':relative_to_cwd(Path);
    false -> Path
  catch
    _:_ -> Path
  end.

split_last([])           -> {[], []};
split_last(List)         -> split_last(List, []).
split_last([H], Acc)     -> {lists:reverse(Acc), H};
split_last([H | T], Acc) -> split_last(T, [H | Acc]).

read_file_type(File) ->
  read_file_type(File, []).

read_file_type(File, Opts) ->
  case file:read_file_info(File, [{time, posix} | Opts]) of
    {ok, #file_info{type=Type}} -> {ok, Type};
    {error, _} = Error -> Error
  end.

%% Boolean checks

returns_boolean(Bool) when is_boolean(Bool) -> true;

returns_boolean({{'.', _, [erlang, Op]}, _, [_]}) when Op == 'not' -> true;

returns_boolean({{'.', _, [erlang, Op]}, _, [_, _]}) when
  Op == 'and'; Op == 'or'; Op == 'xor';
  Op == '==';  Op == '/='; Op == '=<';  Op == '>=';
  Op == '<';   Op == '>';  Op == '=:='; Op == '=/=' -> true;

returns_boolean({{'.', _, [erlang, Op]}, _, [_, Right]}) when
  Op == 'andalso'; Op == 'orelse' ->
  returns_boolean(Right);

returns_boolean({{'.', _, [erlang, Fun]}, _, [_]}) when
  Fun == is_atom;   Fun == is_binary;   Fun == is_bitstring; Fun == is_boolean;
  Fun == is_float;  Fun == is_function; Fun == is_integer;   Fun == is_list;
  Fun == is_number; Fun == is_pid;      Fun == is_port;      Fun == is_reference;
  Fun == is_tuple;  Fun == is_map;      Fun == is_process_alive -> true;

returns_boolean({{'.', _, [erlang, Fun]}, _, [_, _]}) when
  Fun == is_function; Fun == is_record -> true;

returns_boolean({{'.', _, [erlang, Fun]}, _, [_, _, _]}) when
  Fun == function_exported; Fun == is_record -> true;

returns_boolean({'case', _, [_, [{do, Clauses}]]}) ->
  lists:all(fun
    ({'->', _, [_, Expr]}) -> returns_boolean(Expr)
  end, Clauses);

returns_boolean({'cond', _, [[{do, Clauses}]]}) ->
  lists:all(fun
    ({'->', _, [_, Expr]}) -> returns_boolean(Expr)
  end, Clauses);

returns_boolean({'__block__', _, Exprs}) ->
  returns_boolean(lists:last(Exprs));

returns_boolean(_) -> false.
