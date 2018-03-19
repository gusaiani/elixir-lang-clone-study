%% Convenience functions used throughout elixir source code
%% for ast manipulation and querying.
-module(elixir_utils).
-export([get_line/1, split_last/1, noop/0,
         relative_to_cwd/1,]).
-include("elixir.hrl").
-include_lib("kernel/include/file.hrl").

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
