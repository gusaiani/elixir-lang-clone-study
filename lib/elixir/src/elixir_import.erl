%% Module responsible for handling imports and conflicts
%% between local functions and imports.
%% For imports dispatch, please check elixir_dispatch.
-module(elixir_import).
-export([import/4, special_form/2, format_error/1]).
-include("elixir.hrl").
