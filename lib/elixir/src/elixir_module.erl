-module(elixir_module).
-export([data_table/1, defs_table/1, is_open/1, delete_doc/6,
         compile/4, expand_callback/6, format_error/1,
         compiler_modules/0, delete_impl/6,
         write_cache/3, read_cache/2]).
-include("elixir.hrl").
