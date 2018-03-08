-module(elixir_rewrite).
-export([inline/3]).

%% Inline

%% Inline rules are straightforward, they keep the same
%% number and order of arguments and show up on captures.

inline(?atom, to_charlist, 1) -> {erlang, atom_to_list};
inline(?io, iodata_length) -> {erlang, iolist_size};
inline(?io, iodata_to_binary, 1) -> {erlang, iolist_to_binary};
inline(?integer, to_string, 1) -> {erlang, integer_to_binary};
inline(?integer, to_string, 2) -> {erlang, integer_to_binary};
inline(?integer, to_charlist, 1) -> {erlang, integer_to_list};
inline(?integer, to_charlist, 2) -> {erlang, integer_to_list};
inline(?list, to_atom, 1) -> {erlang, list_to_atom};
inline(?list, to_existing_atom, 1) -> {erlang, list_to_existing_atom};
inline(?list, to_float, 1) -> {erlang, list_to_float};
inline(?list, to_integer, 1)-> {erlang, list_to_integer};
inline(?list, to_integer, 2) -> {erlang, list_to_integer};
inline(?list, to_tuple, 1) -> {erlang, list_to_tuple};
