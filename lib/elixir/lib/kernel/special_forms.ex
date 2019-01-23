defmodule Kernel.SpecialForms do
  @moduledoc """
  Special forms are the basic buinding blocks of Elixir, and therefore
  cannot be overridden by the developer.

  We define them in this module. Some of these forms are lexical (like
  `alias/2`, `case/2`, etc.) The macros `{}/1` and `<<>>/1` are also special
  forms used to define tuple and binary data structures respectively.

  This module also documents macros that returns information about Elixir's
  compilation environment, such as (`__ENV__/0`, `__MODULE/0`, `__DIR__/0`, and `__CALLER__/0`).

  Finally, it also documents two special forms, `__block__/1` and
  `__aliases/1`, which are not intended to be called directly by the
  developer but they appear in quoted contents since they are essential
  in Elixir's constructs.
  """

  defmacrop error!(args) do
    quote do
      _ = unquote(args)

      message =
        "Elixir's special forms are expanded by the compiler and must not be invoked directly"

      :erlang.error(RuntimeError.exception(message))
    end
  end

  @doc """
  Creates a tuple.

  More information about the tuple data type and about functions to manipulate
  tuples can be found in the `Tuple` module; some functions for working with
  tuples are also available in `Kernel` (such as `Kernel.elem/2` or
  `Kernel.tuple_size/1`).

  ## AST representation

  Only two-item tuples are considered literals in Elixir and return themselves
  when quoted. Therefore, all other tuples are represented in the AST as calls to
  the `:{}` special form.

      iex> quote do
      ...>   {1, 2}
      ...> end
      {1, 2}

      iex> quote do
      ...>   {1, 2, 3}
        ...> end
      {:{}, [], [1, 2, 3]}

  """
  defmacro unquote(:{})(args), do: error!([args])
end
