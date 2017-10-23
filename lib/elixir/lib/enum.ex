defprotocol Enumerable do
  @moduledoc """
  Enumerable protocol used by `Enum` and `Stream` modules.

  When you invoke a function in the `Enum` module, the first argument
  is usually a collection that must implement this protocol.
  For example, the expression:

      Enum.map([1, 2, 3], &(&1 * 2))

  invokes `Enumerable.reduce/3` to perform the reducing operation that
  builds a mapped list by calling the mapping function `&(&1 * 2)` on
  every element in the collection and consuming the element with an
  accumulated list.

  Internally, `Enum.map/2` is implemented as follows:

      def map(enum, fun) do
        reducer = fn x, acc -> {:cont, [fun.(x) | acc]} end
        Enumerable.reduce(enum, {:cont, []}, reducer) |> elem(1) |> :lists.reverse()
      end

  Notice the user-supplied function is wrapped into a `t:reducer/0` function.
  The `t:reducer/0` function must return a tagged tuple after each step,
  as described in the `t:acc/0` type. At the end, `Enumerable.reduce/3`
  returns `t:result/0`.

  This protocol uses tagged tuples to exchange information between the
  reducer function and the data type that implements the protocol. This
  allows enumeration of resources, such as files, to be done efficiently
  while also guaranteeing the resource will be closed at the end of the
  enumeration. This protocol also allows suspension of the enumeration,
  which is useful when interleaving between many enumerables is required
  (as in zip).

  This protocol requires four functions to be implemented, `reduce/3`,
  `count/1`, `member?/2`, and `slice/1`. The core of the protocol is the
  `reduce/3` function. All other functions exist as optimizations paths
  for data structures that can implement certain properties in better
  than linear time.
  """

  @typedoc """
  The accumulator value for each step.

  It must be a tagged tuple with one of the following "tags":

    * `:cont`    - the enumeration should continue
    * `:halt`    - the enumeration should halt immediately
    * `:suspend` - the enumeration should be suspended immediately

  Depending on the accumulator value, the result returned by
  `Enumerable.reduce/3` will change. Please check the `t:result/0`
  type documentation for more information.

  In case a `t:reducer/0` function returns a `:suspend` accumulator,
  it must be explicitly handled by the caller and never leak.
  """
  @type acc :: {:cont, term} | {:halt, term} | {:suspend, term}

  @typedoc """
  The reducer function.

  Should be called with the enumerable element and the
  accumulator contents.

  Returns the accumulator for the next enumeration step.
  """
  @type reducer :: (term, term -> acc)

  @typedoc """
  The result of the reduce operation.

  It may be *done* when the enumeration is finished by reaching
  its end, or *halted*/*suspended* when the enumeration was halted
  or suspended by the `t:reducer/0` function.

  In case a `t:reducer/0` function returns the `:suspend` accumulator, the
  `:suspended` tuple must be explicitly handled by the caller and
  never leak. In practice, this means regular enumeration functions
  just need to be concerned about `:done` and `:halted` results.

  Furthermore, a `:suspend` call must always be followed by another call,
  eventually halting or continuing until the end.
  """
  @type result ::
          {:done, term}
          | {:halted, term}
          | {:suspended, term, continuation}

  @typedoc """
  A partially applied reduce function.

  The continuation is the closure returned as a result when
  the enumeration is suspended. When invoked, it expects
  a new accumulator and it returns the result.

  A continuation can be trivially implemented as long as the reduce
  function is defined in a tail recursive fashion. If the function
  is tail recursive, all the state is passed as arguments, so
  the continuation is the reducing function partially applied.
  """
  @type continuation :: (acc -> result)

  @typedoc """
  A slicing function that receives the initial position and the
  number of elements in the slice.

  The `start` position is a number `>= 0` and guaranteed to
  exist in the enumerable. The length is a number `>= 1` in a way
  that `start + length <= count`, where `count` is the maximum
  amount of elements in the enumerable.

  The function should return a non empty list where
  the amount of elements is equal to `length`.
  """
  @type slicing_fun :: (start :: non_neg_integer, length :: pos_integer -> [term()])

  @doc """
  Reduces the enumerable into an element.

  Most of the operations in `Enum` are implemented in terms of reduce.
  This function should apply the given `t:reducer/0` function to each
  item in the enumerable and proceed as expected by the returned
  accumulator.

  See the documentation of the types `t:result/0` and `t:acc/0` for
  more information.

  ## Examples

  As an example, here is the implementation of `reduce` for lists:

      def reduce(_,       {:halt, acc}, _fun),   do: {:halted, acc}
      def reduce(list,    {:suspend, acc}, fun), do: {:suspended, acc, &reduce(list, &1, fun)}
      def reduce([],      {:cont, acc}, _fun),   do: {:done, acc}
      def reduce([h | t], {:cont, acc}, fun),    do: reduce(t, fun.(h, acc), fun)

  """
  @spec reduce(t, acc, reducer) :: result
  def reduce(enumerable, acc, fun)

  @doc """
  Retrieves the number of elements in the enumerable.

  It should return `{:ok, count}` if you can count the number of elements
  in the enumerable more efficiently than manually traversing each element
  in the enumerable one by one.

  Otherwise it should return `{:error, __MODULE__}` and a default algorithm
  built on top of `reduce/3` that runs in linear time will be used.
  """
  @spec count(t) :: {:ok, non_neg_integer} | {:error, module}
  def count(enumerable)

  @doc """
  Checks if an element exists within the enumerable.

  It should return `{:ok, boolean}` if you can check the membership of a
  given element in the enumerable with `===` without traversing the whole
  enumerable.

  Otherwise it should return `{:error, __MODULE__}` and a default algorithm
  built on top of `reduce/3` that runs in linear time will be used.
  """
  @spec member?(t, term) :: {:ok, boolean} | {:error, module}
  def member?(enumerable, element)

  @doc """
  Returns a function that slices the data structure contiguously.

  It should return `{:ok, size, slicing_fun}` if the enumerable has
  a known bound and can access a position in the enumerable without
  traversing all previous elements.

  Otherwise it should return `{:error, __MODULE__}` and a default
  algorithm built on top of `reduce/3` that runs in linear time will be
  used.

  ## Differences to `count/1`

  The `size` value returned by this function is used for boundary checks,
  therefore it is extremely important that this function only returns `:ok`
  if retrieving the `size` of the enumerable is cheap, fast and takes constant
  time. Otherwise the simplest of operations, such as `Enum.at(enumerable, 0)`,
  will become too expensive.

  On the other hand, ``
  """

  @doc """
  Retrieves the enumerable's size.

  It should return `{:ok, size}`.

  If `{:error, __MODULE__}` is returned a default algorithm using
  `reduce` is used. This algorithm runs in linear time.

  Please force use of the default algorithm unless you can implement an
  algorithm that is significantly faster.
  """
  @spec count(t) :: {:ok, non_neg_integer} | {:error, module}
  def count(enumerable)
end

defmodule Enum do
  import Kernel, except: [max: 2, min: 2]

  @moduledoc """
  Provides a set of algorithms that enumerate over enumerables according
  to the `Enumerable` protocol.

      iex> Enum.map([1, 2, 3], fn(x) -> x * 2 end)
      [2, 4, 6]

  Some particular types, like maps, yield a specific format on enumeration.
  For example, the argument is always a `{key, value}` tuple for maps:

      iex> map = %{a: 1, b: 2}
      iex> Enum.map(map, fn {k, v} -> {k, v * 2} end)
      [a: 2, b: 4]

  Note that the functions in the `Enum` module are eager: they always
  start the enumeration of the given enumerable. The `Stream` module
  allows lazy enumeration of enumerables and provides infinite streams.

  Since the majority of the functions in `Enum` enumerate the whole
  enumerable and return a list as result, infinite streams need to
  be carefully used with such functions, as they can potentially run
  forever. For example:

      Enum.each Stream.cycle([1, 2, 3]), &IO.puts(&1)

  """

  @compile :inline_list_funcs

  @type t :: Enumerable.t()
  @type acc :: any
  @type element :: any
  @type index :: integer
  @type default :: any

  # Require Stream.Reducers and its callbacks
  require Stream.Reducers, as: R

  defmacrop skip(acc) do
    acc
  end

  defmacrop next(_, entry, acc) do
    quote(do: [unquote(entry) | unquote(acc)])
  end

  defmacrop acc(head, state, _) do
    quote(do: {unquote(head), unquote(state)})
  end

  defmacrop next_with_acc(_, entry, head, state, _) do
    quote do
      {[unquote(entry) | unquote(head)], unquote(state)}
    end
  end

  @doc """
  Returns true if the given `fun` evaluates to true on all of the items in the enumerable.

  It stops the iteration at the first invocation that returns `false` or `nil`.

  ## Examples

      iex> Enum.all?([2, 4, 6], fn(x) -> rem(x, 2) == 0 end)
      true

      iex> Enum.all?([2, 3, 4], fn(x) -> rem(x, 2) == 0 end)
      false

  If no function is given, it defaults to checking if
  all items in the enumerable are truthy values.

      iex> Enum.all?([1, 2, 3])
      true

      iex> Enum.all?([1, nil, 3])
      false

  """
  @spec all?(t, (element -> as_boolean(term))) :: boolean

  def all?(enumerable, fun \\ fn x -> x end)

  def all?(enumerable, fun) when is_list(enumerable) do
    all_list(enumerable, fun)
  end

  def all?(enumerable, fun) do
    Enumerable.reduce(enumerable, {:cont, true}, fn entry, _ ->
      if fun.(entry), do: {:cont, true}, else: {:halt, false}
    end)
    |> elem(1)
  end

  @doc """
  Returns true if the given `fun` evaluates to true on any of the items in the enumerable.

  It stops the iteration at the first invocation that returns a truthy value (not `false` or `nil`).

  ## Examples

      iex> Enum.any?([2, 4, 6], fn(x) -> rem(x, 2) == 1 end)
      false

      iex> Enum.any?([2, 3, 4], fn(x) -> rem(x, 2) == 1 end)
      true

  If no function is given, it defaults to checking if at least one item
  in the enumerable is a truthy value.

      iex> Enum.any?([false, false, false])
      false

      iex> Enum.any?([false, true, false])
      true

  """
  @spec any?(t, (element -> as_boolean(term))) :: boolean

  def any?(enumerable, fun \\ fn x -> x end)

  def any?(enumerable, fun) when is_list(enumerable) do
    any_list(enumerable, fun)
  end

  def any?(enumerable, fun) do
    Enumerable.reduce(enumerable, {:cont, false}, fn entry, _ ->
      if fun.(entry), do: {:halt, true}, else: {:cont, false}
    end)
    |> elem(1)
  end

  @doc """
  Finds the element at the given `index` (zero-based).

  Returns `default` if `index` is out of bounds.

  A negative `index` can be passed, which means the `enumerable` is
  enumerated once and the `index` is counted from the end (e.g.
  `-1` finds the last element).

  Note this operation takes linear time. In order to access
  the element at index `index`, it will need to traverse `index`
  previous elements.

  ## Examples

      iex> Enum.at([2, 4, 6], 0)
      2

      iex> Enum.at([2, 4, 6], 2)
      6

      iex> Enum.at([2, 4, 6], 4)
      nil

      iex> Enum.at([2, 4, 6], 4, :none)
      :none

  """
  @spec at(t, index, default) :: element | default
  def at(enumerable, index, default \\ nil) do
    case fetch(enumerable, index) do
      {:ok, h} -> h
      :error -> default
    end
  end

  @doc """
  Finds the element at the given `index` (zero-based).

  Returns `{:ok, element}` if found, otherwise `:error`.

  A negative `index` can be passed, which means the `enumerable` is
  enumerated once and the `index` is counted from the end (e.g.
  `-1` fetches the last element).

  Note this operation takes linear time. In order to access
  the element at index `index`, it will need to traverse `index`
  previous elements.

  ## Examples

      iex> Enum.fetch([2, 4, 6], 0)
      {:ok, 2}

      iex> Enum.fetch([2, 4, 6], -3)
      {:ok, 2}

      iex> Enum.fetch([2, 4, 6], 2)
      {:ok, 6}

      iex> Enum.fetch([2, 4, 6], 4)
      :error

  """
  @spec fetch(t, index) :: {:ok, element} | :error
  def fetch(enumerable, index)

  def fetch(enumerable, index) when is_list(enumerable) and is_integer(index) do
    if index < 0 do
      enumerable |> :lists.reverse() |> fetch_list(-index - 1)
    else
      fetch_list(enumerable, index)
    end
  end

  ## Implementations

  ## all?

  defp all_list([h | t], fun) do
    if fun.(h) do
      all_list(t, fun)
    else
      false
    end
  end

  defp all_list([], _) do
    true
  end

  defp any_list([h | t], fun) do
    if fun.(h) do
      true
    else
      any_list(t, fun)
    end
  end

  defp any_list([], _) do
    false
  end
end
