defmodule Function do
  @moduledoc """
  A set of functions for working with functions.

  There are two types of captured functions: **external** and **local**.
  External functions are functions residing in modules that are captured
  with `&/1`, such as `&String.length/1`. Local functions are anonymous functions
  defined with `fn/1` or with the capture operator `&/1` using `&1`, `&2`,
  and so on as replacements.
  """

  @type information ::
          :arity
          | :env
          | :index
          | :module
          | :name
          | :new_index
          | :new_uniq
          | :pid
          | :type
          | :uniq

  @doc """
  Captures the given function.

  Inlined by the compiler.

  ## Examples

      iex> Function.capture(String, :length, 1)
      &String.length/1

  """
  @doc since: "1.7.0"
  @spec capture(module, atom, arity) :: fun
  def capture(module, function_name, arity) do
    :erlang.make_fun(module, function_name, arity)
  end
end
