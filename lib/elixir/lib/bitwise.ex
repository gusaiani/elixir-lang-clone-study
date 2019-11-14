defmodule Bitwise do
  @moduledoc """
  A set of functions that perform calculations on bits.

  The functions in this module come in two flavors: named or
  operators. For example:

      iex> use Bitwise
      iex> bnot(1) # named
      -2
      iex> 1 &&& 1 # operator
      1

  If you prefer to use only operators or skip them, you can
  pass the following options:

    * `:only_operators` - includes only operators
    * `:skip_operators` - skips operators

  For example:

      iex> use Bitwise, only_operators: true
      iex> 1 &&& 1
      1

  When invoked with no options, `use Bitwise` is equivalent
  to `import Bitwise`.

  All bitwise functions can be used in guards:

      iex> use Bitwise
      iex> odd? = fn
      ...>   int when band(int, 1) == 1 -> true
      ...>   _ -> false
      ...> end
      iex> odd?.(1)
      true

  All functions in this module are inlined by the compiler.
  """

  @doc false
  defmacro __using__(options) do
    except =
      cond do
        Keyword.get(options, :only_operators) ->
          [bnot: 1, band: 2, bor: 2, bxor: 2, bsl: 2, bsr: 2]

        Keyword.get(options, :skip_operators) ->
          [~~~:1, &&&: 2, |||: 2, ^^^: 2, <<<: 2, >>>: 2]

        true ->
          []
      end

    quote do
      import Bitwise, except: unquote(except)
    end
  end

  @doc """
  Calculates the bitwise NOT of its argument.

  Allowed in guard tests. Inlined by the compiler.

      iex> bnot(2)
      -3
      iex> bnot(2) &&& 3
      1

  """
  @doc guard: true
  def bnot(expr) do
    :erlang.bnot(expr)
  end
end
