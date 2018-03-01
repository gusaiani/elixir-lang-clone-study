%% Handle code related to args, guard and -> matching for case,
%% fn, receive and friends. try is handled in elixir_try.
-module(elixir_clauses).
-export([match/3, clause/5, def/2, head/2,
         'case'/3, 'receive'/3, 'try'/3, 'cond'/3, with/3,
         format_error/1]).
-import(elixir_errors, [form_error/4]).
