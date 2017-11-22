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
