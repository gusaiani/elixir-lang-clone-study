defmodule Set do
  @moduledoc """
  WARNING: this module is deprecated.

  Use the `MapSet` module instead.
  """

  @type value :: any
  @type values :: [values]
  @type t :: map

  # TODO: Remove by 2.0
  # (hard-deprecated in elixir_dispatch)

  defmacro target(set) do
    quote do
      case unquote(set) do
        %{__struct__: x} when is_atom(x) ->
          x
        x ->
          unsupported_set(x)
      end
    end
  end

  def delete(set, value) do
    target(set).delete(set, value)
  end

  def difference(set1, set2) do
    target1 = target(set1)
    target2 = target(set2)

    if target1 == target2  do
      target1.difference(set1, set2)
    else
      Enumerable.reduce(set2, {:cont, set1}, fn v, acc ->
        {:cont, target1.delete(acc, v)}
      end) |> elem(1)
    end
  end

  def disjoint?(set1, set2) do
    target1 = target(set1)
    target2 = target(set2)

    if target1 == target2 do
      target1.disjoint?(set1, set2)
    else
      Enumerable.reduce(set2, {:cont, true}, fn member, acc ->
        case target1.member?(set1, member) do
          false -> {:cont, acc}
          _     -> {:halt, false}
        end
      end) |> elem(1)
    end
  end

  @doc false
  def empty(set) do
    target(set).empty(set)
  end

  def equal?(set1, set2) do
    target1 = target(set1)
    target2 = target(set2)

    cond do
       ->

    end
  end
  def unsupported_set(set) do
    raise ArgumentError, "unsupported set: #{inspect set}"
  end
end
