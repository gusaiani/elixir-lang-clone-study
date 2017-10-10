defmodule Keyword do
  @moduledoc """
  A set of functions for working with keywords.

  A keyword is a list of two-element tuples where the first
  element of the tuple is an atom and the second element
  can be any value.

  For example, the following is a keyword list:

      [{:exit_on_close, true}, {:active, :once}, {:packet_size, 1024}]

  Elixir provides a special and more concise syntax for keyword lists
  that looks like this:

      [exit_on_close: true, active: :once, packet_size: 1024]

  This is also the syntax that Elixir uses to inspect keyword lists:

      iex> [{:active, :once}]
      [active: :once]

  The two syntaxes are completely equivalent. Note that when keyword
  lists are passed as the last argument to a function, if the short-hand
  syntax is used then the square brackets around the keyword list can
  be omitted as well. For example, the following:

      String.split("1-0", "-", trim: true, parts: 2)

  is equivalent to:

      String.split("1-0", "-", [trim: true, parts: 2])

  A keyword may have duplicated keys so it is not strictly
  a key-value store. However most of the functions in this module
  behave exactly as a dictionary so they work similarly to
  the functions you would find in the `Map` module.

  For example, `Keyword.get/3` will get the first entry matching
  the given key, regardless if duplicated entries exist.
  Similarly, `Keyword.put/3` and `Keyword.delete/3` ensure all
  duplicated entries for a given key are removed when invoked.
  Note that operations that require keys to be found in the keyword
  list (like `Keyword.get/3`) need to traverse the list in order
  to find keys, so these operations may be slower than their map
  counterparts.

  A handful of functions exist to handle duplicated keys, in
  particular, `Enum.into/2` allows creating new keywords without
  removing duplicated keys, `get_values/2` returns all values for
  a given key and `delete_first/2` deletes just one of the existing
  entries.

  The functions in `Keyword` do not guarantee any property when
  it comes to ordering. However, since a keyword list is simply a
  list, all the operations defined in `Enum` and `List` can be
  applied too, especially when ordering is required.
  """

  @compile :inline_list_funcs

  @type key :: atom
  @type value :: any

  @type t :: [{key, value}]
  @type t(value) :: [{key, value}]

  @doc """
  Returns `true` if `term` is a keyword list; otherwise returns `false`.

  ## Examples

      iex> Keyword.keyword?([])
      true
      iex> Keyword.keyword?([a: 1])
      true
      iex> Keyword.keyword?([{Foo, 1}])
      true
      iex> Keyword.keyword?([{}])
      false
      iex> Keyword.keyword?([:key])
      false
      iex> Keyword.keyword?(%{})
      false

  """
  @spec keyword?(term) :: boolean
  def keyword?(term)

  def keyword?([{key, _value} | rest]) when is_atom(key), do: keyword?(rest)
  def keyword?([]),     do: true
  def keyword?(_other), do: false

  @doc """
  Returns an empty keyword list, i.e. an empty list.

  ## Examples

      iex> Keyword.new()
      []

  """
  @spec new :: []
  def new, do: []

  @doc """
  Creates a keyword from an enumerable.

  Duplicated entries are removed, the latest one prevails.
  Unlike `Enum.into(enumerable, [])`, `Keyword.new(enumerable)`
  guarantees the keys are unique.

  ## Examples

      iex> Keyword.new([{:b, 1}, {:a, 2}])
      [b: 1, a: 2]

      iex> Keyword.new([{:a, 1}, {:a, 2}, {:a, 3}])
      [a: 3]

  """
  @spec new(Enum.t) :: t
  def new(pairs) do
    new(pairs, fn pair -> pair end)
  end

  @doc """
  Creates a keyword from an enumerable via the transformation function.

  Duplicated entries are removed, the latest one prevails.
  Unlike `Enum.into(enumerable, [], fun)`,
  `Keyword.new(enumerable, fun)` guarantees the keys are unique.

  ## Examples

      iex> Keyword.new([:a, :b], fn(x) -> {x, x} end)
      [a: :a, b: :b]

  """
  @spec new(Enum.t, (term -> {key, value})) :: t
  def new(pairs, transform) do
    fun = fn el, acc ->
      {k, v} = transform.(el)
      put_new(acc, k, v)
    end
    :lists.foldl(fun, [], Enum.reverse(pairs))
  end

  @doc """
  Gets the value for a specific `key`.

  If `key` does not exist, return the default value
  (`nil` if no default value).

  If duplicated entries exist, the first one is returned.
  Use `get_values/2` to retrieve all entries.

  ## Examples

      iex> Keyword.get([], :a)
      nil
      iex> Keyword.get([a: 1], :a)
      1
      iex> Keyword.get([a: 1], :b)
      nil
      iex> Keyword.get([a: 1], :b, 3)
      3

  With duplicated keys:

      iex> Keyword.get([a: 1, a: 2], :a, 3)
      1
      iex> Keyword.get([a: 1, a: 2], :b, 3)
      3

  """
  @spec get(t, key, value) :: value
  def get(keywords, key, default \\ nil) when is_list(keywords) and is_atom(key) do
    case :lists.keyfind(key, 1, keywords) do
      {^key, value} -> value
      false -> default
    end
  end

  @doc """
  Gets the value for a specific `key`.

  If `key` does not exist, lazily evaluates `fun` and returns its result.

  This is useful if the default value is very expensive to calculate or
  generally difficult to setup and teardown again.

  If duplicated entries exist, the first one is returned.
  Use `get_values/2` to retrieve all entries.

  ## Examples

      iex> keyword = [a: 1]
      iex> fun = fn ->
      ...>   # some expensive operation here
      ...>   13
      ...> end
      iex> Keyword.get_lazy(keyword, :a, fun)
      1
      iex> Keyword.get_lazy(keyword, :b, fun)
      13

  """
  @spec get_lazy(t, key, (() -> value)) :: value
  def get_lazy(keywords, key, fun)
      when is_list(keywords) and is_atom(key) and is_function(fun, 0) do
    case :lists.keyfind(key, 1, keywords) do
      {^key, value} -> value
      false -> fun.()
    end
  end
end
