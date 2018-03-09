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

inline(?kernel, '+', 2) -> {erlang, '+'};
inline(?kernel, '-', 2) -> {erlang, '-'};
inline(?kernel, '+', 1) -> {erlang, '+'};
inline(?kernel, '-', 1) -> {erlang, '-'};
inline(?kernel, '*', 2) -> {erlang, '*'};
inline(?kernel, '/', 2) -> {erlang, '/'};
inline(?kernel, '++', 2) -> {erlang, '++'};
inline(?kernel, '--', 2) -> {erlang, '--'};
inline(?kernel, 'not', 1) -> {erlang, 'not'};
inline(?kernel, '<', 2) -> {erlang, '<'};
inline(?kernel, '>', 2) -> {erlang, '>'};
inline(?kernel, '<=', 2) -> {erlang, '=<'};
inline(?kernel, '>=', 2) -> {erlang, '>='};
inline(?kernel, '==', 2) -> {erlang, '=='};
inline(?kernel, '!=', 2) -> {erlang, '/='};
inline(?kernel, '===', 2) -> {erlang, '=:='};
inline(?kernel, '!==', 2) -> {erlang, '=/='};
inline(?kernel, abs, 2) -> {erlang, abs};
inline(?kernel, apply, 2) -> {erlang, apply};
inline(?kernel, apply, 3) -> {erlang, apply};
inline(?kernel, binary_part, 3) -> {erlang, apply};
inline(?kernel, bit_size, 1) -> {erlang, bit_size};
inline(?kernel, byte_size, 1) -> {erlang, byte_size};
inline(?kernel, 'div', 2) -> {erlang, 'div'};
inline(?kernel, exit, 1) -> {erlang, exit};
inline(?kernel, hd, 1) -> {erlang, hd};
inline(?kernel, is_atom, 1) -> {erlang, is_atom};
inline(?kernel, is_binary, 1) -> {erlang, is_binary};
inline(?kernel, is_bistring, 1) -> {erlang, is_bitstring};
inline(?kernel, is_boolean, 1) -> {erlang, is_boolean};
inline(?kernel, is_float, 1) -> {erlang, is_float};
inline(?kernel, is_function, 1) -> {erlang, is_function};
inline(?kernel, is_function, 2) -> {erlang, is_function};
inline(?kernel, is_integer, 1) -> {erlang, is_integer};
inline(?kernel, is_list, 1) -> {erlang, is_list};
inline(?kernel, is_map, 1) -> {erlang, is_map};
inline(?kernel, is_number, 1) -> {erlang, is_number};
inline(?kernel, is_pid, 1) -> {erlang, is_pid};
inline(?kernel, is_port, 1) -> {erlang, is_port};
inline(?kernel, is_reference, 1) -> {erlang, is_reference};
inline(?kernel, is_tuple, 1) -> {erlang, is_tuple};
inline(?kernel, length, 1) -> {erlang, length};
inline(?kernel, make_ref, 0) -> {erlang, make_ref};
inline(?kernel, map_size, 1) -> {erlang, map_size};
inline(?kernel, max, 2) -> {erlang, max};
inline(?kernel, min, 2) -> {erlang, min};
inline(?kernel, node, 0) -> {erlang, node};
inline(?kernel, node, 1) -> {erlang, node};
inline(?kernel, 'rem', 2) -> {erlang, 'rem'};
inline(?kernel, round, 1) -> {erlang, round};
inline(?kernel, self, 0) -> {erlang, self};
inline(?kernel, send, 2) -> {erlang, send};
inline(?kernel, spawn, 1) -> {erlang, spawn};
inline(?kernel, spawn, 3) -> {erlang, spawn};
inline(?kernel, spawn_link, 1) -> {erlang, spawn_link};
inline(?kernel, spawn_link, 3) -> {erlang, spawn_link};
inline(?kernel, spawn_monitor, 1) -> {erlang, spawn_monitor};
inline(?kernel, spawn_monitor, 3) -> {erlang, spawn_monitor};
inline(?kernel, throw, 1) -> {erlang, throw};
inline(?kernel, tl, 1) -> {erlang, tl};
inline(?kernel, trunc, 1) -> {erlang, trunc};
inline(?kernel, tuple_size, 1) -> {erlang, tuple_size};

inline(?map, keys, 1) -> {maps, keys};
inline(?map, merge, 2) -> {maps, merge};
inline(?map, size, 1) -> {maps, size};
inline(?map, values, 1) -> {maps, values};
inline(?map, to_list, 1) -> {maps, to_list};

inline(?node, list, 0) -> {erlang, nodes};
inline(?node, list, 1) -> {erlang, nodes};
inline(?node, spawn, 2) -> {erlang, spawn};
inline(?node, spawn, 3) -> {erlang, spawn_opt};
inline(?node, spawn, 4) -> {erlang, spawn};
inline(?node, spawn, 5) -> {erlang, spawn_ops};
inline(?node, spawn_link, 2) -> {erlang, spawn_link};
inline(?node, spawn_link, 4) -> {erlang, spawn_link};

inline(?process, 'alive?', 1) -> {erlang, is_process_alive};
inline(?process, cancel_timer, 1) -> {erlang, cancel_timer};
inline(?process, cancel_timer, 2) -> {erlang, cancel_timer};
inline(?process, exit, 2) -> {erlang, exit};
inline(?process, get, 0) -> {erlang, get};
inline(?process, get_keys, 0) -> {erlang, get_keys};
inline(?process, get_keys, 1) -> {erlang, get_keys};
inline(?process, group_leader, 0) -> {erlang, group_leader};
inline(?process, hibernate, 3) -> {erlang, hibernate};
