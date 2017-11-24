defmodule Stream do
  @moduledoc """
  Module for creating and composing streams.

  Streams are composable, lazy enumerables. Any enumerable that generates
  items one by one during enumeration is called a stream. For example,
  Elixir's `Range` is a stream:

      iex> range = 1..5
      1..5
      iex> Enum.map range, &(&1 * 2)
      [2, 4, 6, 8, 10]

  In the example above, as we mapped over the range, the elements being
  enumerated were created one by one, during enumeration. The `Stream`
  module allows us to map the range, without triggering its enumeration:

      iex> range = 1..3
      iex> stream = Stream.map(range, &(&1 * 2))
      iex> Enum.map(stream, &(&1 + 1))
      [3, 5, 7]

  Notice we started with a range and then we created a stream that is
  meant to multiply each item in the range by 2. At this point, no
  computation was done. Only when `Enum.map/2` is called we actually
  enumerate over each item in the range, multiplying it by 2 and adding 1.
  We say the functions in `Stream` are *lazy* and the functions in `Enum`
  are *eager*.

  Due to their laziness, streams are useful when working with large
  (or even infinite) collections. When chaining many operations with `Enum`,
  intermediate lists are created, while `Stream` creates a recipe of
  computations that are executed at a later moment. Let's see another
  example:

      1..3
      |> Enum.map(&IO.inspect(&1))
      |> Enum.map(&(&1 * 2))
      |> Enum.map(&IO.inspect(&1))
      1
      2
      3
      2
      4
      6
      #=> [2, 4, 6]

  Notice that we first printed each item in the list, then multiplied each
  element by 2 and finally printed each new value. In this example, the list
  was enumerated three times. Let's see an example with streams:

      stream = 1..3
      |> Stream.map(&IO.inspect(&1))
      |> Stream.map(&(&1 * 2))
      |> Stream.map(&IO.inspect(&1))
      Enum.to_list(stream)
      1
      2
      2
      4
      3
      6
      #=> [2, 4, 6]

  Although the end result is the same, the order in which the items were
  printed changed! With streams, we print the first item and then print
  its double. In this example, the list was enumerated just once!

  That's what we meant when we said earlier that streams are composable,
  lazy enumerables. Notice we could call `Stream.map/2` multiple times,
  effectively composing the streams and keeping them lazy. The computations
  are only performed when you call a function from the `Enum` module.

  ## Creating Streams

  There are many functions in Elixir's standard library that return
  streams, some examples are:

    * `IO.stream/2`         - streams input lines, one by one
    * `URI.query_decoder/1` - decodes a query string, pair by pair

  This module also provides many convenience functions for creating streams,
  like `Stream.cycle/1`, `Stream.unfold/2`, `Stream.resource/3` and more.

  Note the functions in this module are guaranteed to return enumerables.
  Since enumerables can have different shapes (structs, anonymous functions,
  and so on), the functions in this module may return any of those shapes
  and this may change at any time. For example, a function that today
  returns an anonymous function may return a struct in future releases.
  """

  @doc false
  defstruct enum: nil, funs: [], accs: [], done: nil

  @type acc :: any
  @type element :: any
  @type index :: non_neg_integer
  @type default :: any

  # Require Stream.Reducers and its callbacks
  require Stream.Reducers, as: R

  defmacrop skip(acc) do
    {:cont, acc}
  end

  defmacrop next(fun, entry, acc) do
    quote(do: unquote(fun).(unquote(entry), unquote(acc)))
  end

  defmacrop acc(head, state, tail) do
    quote(do: [unquote(head), unquote(state) | unquote(tail)])
  end

  defmacrop next_with_acc(fun, entry, head, state, tail) do
    quote do
      {reason, [head | tail]} = unquote(fun).(unquote(entry), [unquote(head) | unquote(tail)])
      {reason, [head, unquote(state) | tail]}
    end
  end

  ## Transformers

  # Deprecate on v1.7
  @doc false
  def chunk(enum, n), do: chunk(enum, n, n, nil)

  # Deprecate on v1.7
  @doc false
  def chunk(enum, n, step, leftover \\ nil)
      when is_integer(n) and n > 0 and is_integer(step) and step > 0 do
    chunk_every(enum, n, step, leftover || :discard)
  end

  @doc """
  Shortcut to `chunk_every(enum, count, count)`.
  """
  @spec chunk_every(Enumerable.t(), pos_integer) :: Enumerable.t()
  def chunk_every(enum, count), do: chunk_every(enum, count, count, [])

  @doc """
  Streams the enumerable in chunks, containing `count` items each,
  where each new chunk starts `step` elements into the enumerable.

  `step` is optional and, if not passed, defaults to `count`, i.e.
  chunks do not overlap.

  If the last chunk does not have `count` elements to fill the chunk,
  elements are taken from `leftover` to fill in the chunk. If `leftover`
  does not have enough elements to fill the chunk, then a partial chunk
  is returned with less than `count` elements.

  If `:discard` is given in `leftover`, the last chunk is discarded
  unless it has exactly `count` elements.

  ## Examples

      iex> Stream.chunk_every([1, 2, 3, 4, 5, 6], 2) |> Enum.to_list
      [[1, 2], [3, 4], [5, 6]]

      iex> Stream.chunk_every([1, 2, 3, 4, 5, 6], 3, 2, :discard) |> Enum.to_list
      [[1, 2, 3], [3, 4, 5]]

      iex> Stream.chunk_every([1, 2, 3, 4, 5, 6], 3, 2, [7]) |> Enum.to_list
      [[1, 2, 3], [3, 4, 5], [5, 6, 7]]

      iex> Stream.chunk_every([1, 2, 3, 4, 5, 6], 3, 3, []) |> Enum.to_list
      [[1, 2, 3], [4, 5, 6]]

  """
  @spec chunk_every(Enumerable.t(), pos_integer, pos_integer, Enumerable.t() | :discard) ::
          Enumerable.t()
  def chunk_every(enum, count, step, leftover \\ [])
      when is_integer(count) and count > 0 and is_integer(step) and step > 0 do
    R.chunk_every(&chunk_while/4, enum, count, step, leftover)
  end

  @doc """
  Chunks the `enum` by buffering elements for which `fun` returns the same value.

  Elements are only emitted when `fun` returns a new value or the `enum` finishes.

  ## Examples

      iex> stream = Stream.chunk_by([1, 2, 2, 3, 4, 4, 6, 7, 7], &(rem(&1, 2) == 1))
      iex> Enum.to_list(stream)
      [[1], [2, 2], [3], [4, 4, 6], [7, 7]]

  """
  @spec chunk_by(Enumerable.t(), (element -> any)) :: Enumerable.t()
  def chunk_by(enum, fun) do
    R.chunk_by(&chunk_while/4, enum, fun)
  end

  @doc """
  Chunks the `enum` with fine grained control when every chunk is emitted.

  `chunk_fun` receives the current element and the accumulator and
  must return `{:cont, element, acc}` to emit the given chunk and
  continue with accumulator or `{:cont, acc}` to not emit any chunk
  and continue with the return accumulator.

  `after_fun` is invoked when iteration is done and must also return
  `{:cont, element, acc}` or `{:cont, acc}`.

  ## Examples

      iex> chunk_fun = fn i, acc ->
      ...>   if rem(i, 2) == 0 do
      ...>     {:cont, Enum.reverse([i | acc]), []}
      ...>   else
      ...>     {:cont, [i | acc]}
      ...>   end
      ...> end
      iex> after_fun = fn
      ...>   [] -> {:cont, []}
      ...>   acc -> {:cont, Enum.reverse(acc), []}
      ...> end
      iex> stream = Stream.chunk_while(1..10, [], chunk_fun, after_fun)
      iex> Enum.to_list(stream)
      [[1, 2], [3, 4], [5, 6], [7, 8], [9, 10]]

  """
  @spec chunk_while(
          Enumerable.t(),
          acc,
          (element, acc -> {:cont, chunk, acc} | {:cont, acc} | {:halt, acc}),
          (acc -> {:cont, chunk, acc} | {:cont, acc})
        ) :: Enumerable.t()
        when chunk: any
  def chunk_while(enum, acc, chunk_fun, after_fun) do
    lazy(
      enum,
      acc,
      fn f1 -> R.chunk_while(chunk_fun, f1) end,
      &after_chunk_while(&1, &2, after_fun)
    )
  end

  defp after_chunk_while(acc(h, acc, t), f1, after_fun) do
    case after_fun.(acc) do
      {:cont, emit, acc} -> next_with_acc(f1, emit, h, acc, t)
      {:cont, acc} -> {:cont, acc(h, acc, t)}
    end
  end

  @doc """
  Creates a stream that only emits elements if they are different from the last emitted element.

  This function only ever needs to store the last emitted element.

  Elements are compared using `===`.

  ## Examples

      iex> Stream.dedup([1, 2, 3, 3, 2, 1]) |> Enum.to_list
      [1, 2, 3, 2, 1]

  """
  @spec dedup(Enumerable.t()) :: Enumerable.t()
  def dedup(enum) do
    dedup_by(enum, fn x -> x end)
  end

  @doc """
  Creates a stream that only emits elements if the result of calling `fun` on the element is
  different from the (stored) result of calling `fun` on the last emitted element.

  ## Examples

      iex> Stream.dedup_by([{1, :x}, {2, :y}, {2, :z}, {1, :x}], fn {x, _} -> x end) |> Enum.to_list
      [{1, :x}, {2, :y}, {1, :x}]

  """
  @spec dedup_by(Enumerable.t(), (element -> term)) :: Enumerable.t()
  def dedup_by(enum, fun) do
    lazy(enum, nil, fn f1 -> R.dedup(fun, f1) end)
  end

  @doc """
  Lazily drops the next `n` items from the enumerable.

  If a negative `n` is given, it will drop the last `n` items from
  the collection. Note that the mechanism by which this is implemented
  will delay the emission of any item until `n` additional items have
  been emitted by the enum.

  ## Examples

      iex> stream = Stream.drop(1..10, 5)
      iex> Enum.to_list(stream)
      [6, 7, 8, 9, 10]

      iex> stream = Stream.drop(1..10, -5)
      iex> Enum.to_list(stream)
      [1, 2, 3, 4, 5]

  """
  @spec drop(Enumerable.t(), non_neg_integer) :: Enumerable.t()
  def drop(enum, n) when n >= 0 do
    lazy(enum, n, fn f1 -> R.drop(f1) end)
  end

  def drop(enum, n) when n < 0 do
    n = abs(n)

    lazy(enum, {0, [], []}, fn f1 ->
      fn
        entry, [h, {count, buf1, []} | t] ->
          do_drop(:cont, n, entry, h, count, buf1, [], t)

        entry, [h, {count, buf1, [next | buf2]} | t] ->
          {reason, [h | t]} = f1.(next, [h | t])
          do_drop(reason, n, entry, h, count, buf1, buf2, t)
      end
    end)
  end

  defp do_drop(reason, n, entry, h, count, buf1, buf2, t) do
    buf1 = [entry | buf1]
    count = count + 1

    if count == n do
      {reason, [h, {0, [], :lists.reverse(buf1)} | t]}
    else
      {reason, [h, {count, buf1, buf2} | t]}
    end
  end

  @doc """
  Creates a stream that drops every `nth` item from the enumerable.

  The first item is always dropped, unless `nth` is 0.

  `nth` must be a non-negative integer.

  ## Examples

      iex> stream = Stream.drop_every(1..10, 2)
      iex> Enum.to_list(stream)
      [2, 4, 6, 8, 10]

      iex> stream = Stream.drop_every(1..1000, 1)
      iex> Enum.to_list(stream)
      []

      iex> stream = Stream.drop_every([1, 2, 3, 4, 5], 0)
      iex> Enum.to_list(stream)
      [1, 2, 3, 4, 5]

  """
  @spec drop_every(Enumerable.t(), non_neg_integer) :: Enumerable.t()
  def drop_every(enum, nth)
  def drop_every(enum, 0), do: %Stream{enum: enum}
  def drop_every([], _nth), do: %Stream{enum: []}

  def drop_every(enum, nth) when is_integer(nth) and nth > 0 do
    lazy(enum, nth, fn f1 -> R.drop_every(nth, f1) end)
  end

  @doc """
  Lazily drops elements of the enumerable while the given
  function returns a truthy value.

  ## Examples

      iex> stream = Stream.drop_while(1..10, &(&1 <= 5))
      iex> Enum.to_list(stream)
      [6, 7, 8, 9, 10]

  """
  @spec drop_while(Enumerable.t(), (element -> as_boolean(term))) :: Enumerable.t()
  def drop_while(enum, fun) do
    lazy(enum, true, fn f1 -> R.drop_while(fun, f1) end)
  end

  @doc """
  Executes the given function for each item.

  Useful for adding side effects (like printing) to a stream.

  ## Examples

      iex> stream = Stream.each([1, 2, 3], fn(x) -> send self(), x end)
      iex> Enum.to_list(stream)
      iex> receive do: (x when is_integer(x) -> x)
      1
      iex> receive do: (x when is_integer(x) -> x)
      2
      iex> receive do: (x when is_integer(x) -> x)
      3

  """
  @spec each(Enumerable.t(), (element -> term)) :: Enumerable.t()
  def each(enum, fun) do
    lazy(enum, fn f1 ->
      fn x, acc ->
        fun.(x)
        f1.(x, acc)
      end
    end)
  end

  @doc """
  Maps the given `fun` over `enumerable` and flattens the result.

  This function returns a new stream built by appending the result of invoking `fun`
  on each element of `enumerable` together.

  ## Examples

      iex> stream = Stream.flat_map([1, 2, 3], fn(x) -> [x, x * 2] end)
      iex> Enum.to_list(stream)
      [1, 2, 2, 4, 3, 6]

      iex> stream = Stream.flat_map([1, 2, 3], fn(x) -> [[x]] end)
      iex> Enum.to_list(stream)
      [[1], [2], [3]]

  """
  @spec flat_map(Enumerable.t(), (element -> Enumerable.t())) :: Enumerable.t()
  def flat_map(enum, mapper) do
    transform(enum, nil, fn val, nil -> {mapper.(val), nil} end)
  end

  @doc """
  Creates a stream that filters elements according to
  the given function on enumeration.

  ## Examples

      iex> stream = Stream.filter([1, 2, 3], fn(x) -> rem(x, 2) == 0 end)
      iex> Enum.to_list(stream)
      [2]

  """
  @spec filter(Enumerable.t(), (element -> as_boolean(term))) :: Enumerable.t()
  def filter(enum, fun) do
    lazy(enum, fn f1 -> R.filter(fun, f1) end)
  end

  @doc false
  # TODO: Remove on 2.0
  # (hard-deprecated in elixir_dispatch)
  def filter_map(enum, filter, mapper) do
    lazy(enum, fn f1 -> R.filter_map(filter, mapper, f1) end)
  end

  @doc """
  Creates a stream that emits a value after the given period `n`
  in milliseconds.

  The values emitted are an increasing counter starting at `0`.
  This operation will block the caller by the given interval
  every time a new item is streamed.

  Do not use this function to generate a sequence of numbers.
  If blocking the caller process is not necessary, use
  `Stream.iterate(0, & &1 + 1)` instead.

  ## Examples

      iex> Stream.interval(10) |> Enum.take(10)
      [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]

  """
  @spec interval(non_neg_integer) :: Enumerable.t()
  def interval(n) do
    unfold(0, fn count ->
      Process.sleep(n)
      {count, count + 1}
    end)
  end
