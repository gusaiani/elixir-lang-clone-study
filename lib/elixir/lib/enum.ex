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
  in the enumerable.

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

  On the other hand, the `count/1` function in this protocol should be
  implemented whenever you can count the number of elements in the collection.
  """
  @spec slice(t) ::
          {:ok, size :: non_neg_integer(), slicing_fun()}
          | {:error, module()}
  def slice(enumerable)
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
    case slice_any(enumerable, index, 1) do
      [value] -> value
      [] -> default
    end
  end

  # Deprecate on v1.7
  @doc false
  def chunk(enumerable, count), do: chunk(enumerable, count, count, nil)

  # Deprecate on v1.7
  @doc false
  def chunk(enumerable, count, step, leftover \\ nil) do
    chunk_every(enumerable, count, step, leftover || :discard)
  end

  @doc """
  Shortcut to `chunk_every(enumerable, count, count)`.
  """
  @spec chunk_every(t, pos_integer) :: [list]
  def chunk_every(enumerable, count), do: chunk_every(enumerable, count, count, [])

  @doc """
  Returns list of lists containing `count` items each, where
  each new chunk starts `step` elements into the enumerable.

  `step` is optional and, if not passed, defaults to `count`, i.e.
  chunks do not overlap.

  If the last chunk does not have `count` elements to fill the chunk,
  elements are taken from `leftover` to fill in the chunk. If `leftover`
  does not have enough elements to fill the chunk, then a partial chunk
  is returned with less than `count` elements.

  If `:discard` is given in `leftover`, the last chunk is discarded
  unless it has exactly `count` elements.

  ## Examples

      iex> Enum.chunk_every([1, 2, 3, 4, 5, 6], 2)
      [[1, 2], [3, 4], [5, 6]]

      iex> Enum.chunk_every([1, 2, 3, 4, 5, 6], 3, 2, :discard)
      [[1, 2, 3], [3, 4, 5]]

      iex> Enum.chunk_every([1, 2, 3, 4, 5, 6], 3, 2, [7])
      [[1, 2, 3], [3, 4, 5], [5, 6, 7]]

      iex> Enum.chunk_every([1, 2, 3, 4], 3, 3, [])
      [[1, 2, 3], [4]]

      iex> Enum.chunk_every([1, 2, 3, 4], 10)
      [[1, 2, 3, 4]]

  """
  @spec chunk_every(t, pos_integer, pos_integer, t | :discard) :: [list]
  def chunk_every(enumerable, count, step, leftover \\ [])
      when is_integer(count) and count > 0 and is_integer(step) and step > 0 do
    R.chunk_every(&chunk_while/4, enumerable, count, step, leftover)
  end

  @doc """
  Chunks the `enum` with fine grained control when every chunk is emitted.

  `chunk_fun` receives the current element and the accumulator and
  must return `{:cont, element, acc}` to emit the given chunk and
  continue with accumulator or `{:cont, acc}` to not emit any chunk
  and continue with the return accumulator.

  `after_fun` is invoked when iteration is done and must also return
  `{:cont, element, acc}` or `{:cont, acc}`.

  Returns a list of lists.

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
      iex> Enum.chunk_while(1..10, [], chunk_fun, after_fun)
      [[1, 2], [3, 4], [5, 6], [7, 8], [9, 10]]

  """
  @spec chunk_while(
          t,
          acc,
          (element, acc -> {:cont, chunk, acc} | {:cont, acc} | {:halt, acc}),
          (acc -> {:cont, chunk, acc} | {:cont, acc})
        ) :: Enumerable.t()
        when chunk: any
  def chunk_while(enum, acc, chunk_fun, after_fun) do
    {_, {res, acc}} =
      Enumerable.reduce(enum, {:cont, {[], acc}}, fn entry, {buffer, acc} ->
        case chunk_fun.(entry, acc) do
          {:cont, emit, acc} -> {:cont, {[emit | buffer], acc}}
          {:cont, acc} -> {:cont, {buffer, acc}}
          {:halt, acc} -> {:halt, {buffer, acc}}
        end
      end)

    case after_fun.(acc) do
      {:cont, _acc} -> :lists.reverse(res)
      {:cont, elem, _acc} -> :lists.reverse([elem | res])
    end
  end

  @doc """
  Splits enumerable on every element for which `fun` returns a new
  value.

  Returns a list of lists.

  ## Examples

      iex> Enum.chunk_by([1, 2, 2, 3, 4, 4, 6, 7, 7], &(rem(&1, 2) == 1))
      [[1], [2, 2], [3], [4, 4, 6], [7, 7]]

  """
  @spec chunk_by(t, (element -> any)) :: [list]
  def chunk_by(enumerable, fun) do
    R.chunk_by(&chunk_while/4, enumerable, fun)
  end

  @doc """
  Given an enumerable of enumerables, concatenates the enumerables into
  a single list.

  ## Examples

      iex> Enum.concat([1..3, 4..6, 7..9])
      [1, 2, 3, 4, 5, 6, 7, 8, 9]

      iex> Enum.concat([[1, [2], 3], [4], [5, 6]])
      [1, [2], 3, 4, 5, 6]

  """
  @spec concat(t) :: t
  def concat(enumerables) do
    fun = &[&1 | &2]
    reduce(enumerables, [], &reduce(&1, &2, fun)) |> :lists.reverse()
  end

  @doc """
  Concatenates the enumerable on the right with the enumerable on the
  left.

  This function produces the same result as the `Kernel.++/2` operator
  for lists.

  ## Examples

      iex> Enum.concat(1..3, 4..6)
      [1, 2, 3, 4, 5, 6]

      iex> Enum.concat([1, 2, 3], [4, 5, 6])
      [1, 2, 3, 4, 5, 6]

  """
  @spec concat(t, t) :: t
  def concat(left, right) when is_list(left) and is_list(right) do
    left ++ right
  end

  def concat(left, right) do
    concat([left, right])
  end

  @doc """
  Returns the size of the enumerable.

  ## Examples

      iex> Enum.count([1, 2, 3])
      3

  """
  @spec count(t) :: non_neg_integer
  def count(enumerable) when is_list(enumerable) do
    length(enumerable)
  end

  def count(enumerable) do
    case Enumerable.count(enumerable) do
      {:ok, value} when is_integer(value) ->
        value

      {:error, module} ->
        module.reduce(enumerable, {:cont, 0}, fn _, acc -> {:cont, acc + 1} end) |> elem(1)
    end
  end

  @doc """
  Returns the count of items in the enumerable for which `fun` returns
  a truthy value.

  ## Examples

      iex> Enum.count([1, 2, 3, 4, 5], fn(x) -> rem(x, 2) == 0 end)
      2

  """
  @spec count(t, (element -> as_boolean(term))) :: non_neg_integer
  def count(enumerable, fun) do
    reduce(enumerable, 0, fn entry, acc ->
      if(fun.(entry), do: acc + 1, else: acc)
    end)
  end

  @doc """
  Enumerates the `enumerable`, returning a list where all consecutive
  duplicated elements are collapsed to a single element.

  Elements are compared using `===`.

  If you want to remove all duplicated elements, regardless of order,
  see `uniq/1`.

  ## Examples

      iex> Enum.dedup([1, 2, 3, 3, 2, 1])
      [1, 2, 3, 2, 1]

      iex> Enum.dedup([1, 1, 2, 2.0, :three, :"three"])
      [1, 2, 2.0, :three]

  """
  @spec dedup(t) :: list
  def dedup(enumerable) do
    dedup_by(enumerable, fn x -> x end)
  end

  @doc """
  Enumerates the `enumerable`, returning a list where all consecutive
  duplicated elements are collapsed to a single element.

  The function `fun` maps every element to a term which is used to
  determine if two elements are duplicates.

  ## Examples

      iex> Enum.dedup_by([{1, :a}, {2, :b}, {2, :c}, {1, :a}], fn {x, _} -> x end)
      [{1, :a}, {2, :b}, {1, :a}]

      iex> Enum.dedup_by([5, 1, 2, 3, 2, 1], fn x -> x > 2 end)
      [5, 1, 3, 2]

  """
  @spec dedup_by(t, (element -> term)) :: list
  def dedup_by(enumerable, fun) do
    {list, _} = reduce(enumerable, {[], []}, R.dedup(fun))
    :lists.reverse(list)
  end

  @doc """
  Drops the `amount` of items from the enumerable.

  If a negative `amount` is given, the `amount` of last values will be dropped.
  The `enumerable` will be enumerated once to retrieve the proper index and
  the remaining calculation is performed from the end.

  ## Examples

      iex> Enum.drop([1, 2, 3], 2)
      [3]

      iex> Enum.drop([1, 2, 3], 10)
      []

      iex> Enum.drop([1, 2, 3], 0)
      [1, 2, 3]

      iex> Enum.drop([1, 2, 3], -1)
      [1, 2]

  """
  @spec drop(t, integer) :: list
  def drop(enumerable, amount)
      when is_list(enumerable) and is_integer(amount) and amount >= 0 do
    drop_list(enumerable, amount)
  end

  def drop(enumerable, amount) when is_integer(amount) and amount >= 0 do
    {result, _} = reduce(enumerable, {[], amount}, R.drop())
    if is_list(result), do: :lists.reverse(result), else: []
  end

  def drop(enumerable, amount) when is_integer(amount) and amount < 0 do
    {count, fun} = slice_count_and_fun(enumerable)
    amount = Kernel.min(amount + count, count)

    if amount > 0 do
      fun.(0, amount)
    else
      []
    end
  end

  @doc """
  Returns a list of every `nth` item in the enumerable dropped,
  starting with the first element.

  The first item is always dropped, unless `nth` is 0.

  The second argument specifying every `nth` item must be a non-negative
  integer.

  ## Examples

      iex> Enum.drop_every(1..10, 2)
      [2, 4, 6, 8, 10]

      iex> Enum.drop_every(1..10, 0)
      [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

      iex> Enum.drop_every([1, 2, 3], 1)
      []

  """
  @spec drop_every(t, non_neg_integer) :: list
  def drop_every(enumerable, nth)

  def drop_every(_enumerable, 1), do: []
  def drop_every(enumerable, 0), do: to_list(enumerable)
  def drop_every([], nth) when is_integer(nth), do: []

  def drop_every(enumerable, nth) when is_integer(nth) and nth > 1 do
    {res, _} = reduce(enumerable, {[], :first}, R.drop_every(nth))
    :lists.reverse(res)
  end

  @doc """
  Drops items at the beginning of the enumerable while `fun` returns a
  truthy value.

  ## Examples

      iex> Enum.drop_while([1, 2, 3, 2, 1], fn(x) -> x < 3 end)
      [3, 2, 1]

  """
  @spec drop_while(t, (element -> as_boolean(term))) :: list
  def drop_while(enumerable, fun) when is_list(enumerable) do
    drop_while_list(enumerable, fun)
  end

  def drop_while(enumerable, fun) do
    {res, _} = reduce(enumerable, {[], true}, R.drop_while(fun))
    :lists.reverse(res)
  end

  @doc """
  Invokes the given `fun` for each item in the enumerable.

  Returns `:ok`.

  ## Examples

      Enum.each(["some", "example"], fn(x) -> IO.puts x end)
      "some"
      "example"
      #=> :ok

  """
  @spec each(t, (element -> any)) :: :ok
  def each(enumerable, fun) when is_list(enumerable) do
    :lists.foreach(fun, enumerable)
    :ok
  end

  def each(enumerable, fun) do
    reduce(enumerable, nil, fn entry, _ ->
      fun.(entry)
      nil
    end)

    :ok
  end

  @doc """
  Determines if the enumerable is empty.

  Returns `true` if `enumerable` is empty, otherwise `false`.

  ## Examples

      iex> Enum.empty?([])
      true

      iex> Enum.empty?([1, 2, 3])
      false

  """
  @spec empty?(t) :: boolean
  def empty?(enumerable) when is_list(enumerable) do
    enumerable == []
  end

  def empty?(enumerable) do
    case Enumerable.slice(enumerable) do
      {:ok, value, _} ->
        value == 0

      {:error, module} ->
        module.reduce(enumerable, {:cont, true}, fn _, _ -> {:halt, false} end)
        |> elem(1)
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
  def fetch(enumerable, index) do
    case slice_any(enumerable, index, 1) do
      [value] -> {:ok, value}
      [] -> :error
    end
  end

  @doc """
  Finds the element at the given `index` (zero-based).

  Raises `OutOfBoundsError` if the given `index` is outside the range of
  the enumerable.

  Note this operation takes linear time. In order to access the element
  at index `index`, it will need to traverse `index` previous elements.

  ## Examples

      iex> Enum.fetch!([2, 4, 6], 0)
      2

      iex> Enum.fetch!([2, 4, 6], 2)
      6

      iex> Enum.fetch!([2, 4, 6], 4)
      ** (Enum.OutOfBoundsError) out of bounds error

  """
  @spec fetch!(t, index) :: element | no_return
  def fetch!(enumerable, index) do
    case slice_any(enumerable, index, 1) do
      [value] -> value
      [] -> raise Enum.OutOfBoundsError
    end
  end

  @doc """
  Filters the enumerable, i.e. returns only those elements
  for which `fun` returns a truthy value.

  See also `reject/2` which discards all elements where the
  function returns true.

  ## Examples

      iex> Enum.filter([1, 2, 3], fn(x) -> rem(x, 2) == 0 end)
      [2]

  Keep in mind that `filter` is not capable of filtering and
  transforming an element at the same time. If you would like
  to do so, consider using `flat_map/2`. For example, if you
  want to convert all strings that represent an integer and
  discard the invalid one in one pass:

      strings = ["1234", "abc", "12ab"]
      Enum.flat_map(strings, fn string ->
        case Integer.parse(string) do
          {int, _rest} -> [int] # transform to integer
          :error -> [] # skip the value
        end
      end)

  """
  @spec filter(t, (element -> as_boolean(term))) :: list
  def filter(enumerable, fun) when is_list(enumerable) do
    filter_list(enumerable, fun)
  end

  def filter(enumerable, fun) do
    reduce(enumerable, [], R.filter(fun)) |> :lists.reverse()
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

  ## drop

  defp drop_list(list, 0), do: list
  defp drop_list([_ | tail], counter), do: drop_list(tail, counter - 1)
  defp drop_list([], _), do: []

  ## drop_while

  defp drop_while_list([head | tail], fun) do
    if fun.(head) do
      drop_while_list(tail, fun)
    else
      [head | tail]
    end
  end

  defp drop_while_list([], _) do
    []
  end

  ## slice

  defp slice_any(enumerable, start, amount) when start < 0 do
    {count, fun} = slice_count_and_fun(enumerable)
    start = count + start

    if start >= 0 do
      fun.(start, Kernel.min(amount, count - start))
    else
      []
    end
  end

  defp slice_any(list, start, amount) when is_list(list) do
    Enumerable.List.slice(list, start, amount)
  end

  defp slice_any(enumerable, start, amount) do
    case Enumerable.slice(enumerable) do
      {:ok, count, _} when start >= count ->
        []

      {:ok, count, fun} when is_function(fun) ->
        fun.(start, Kernel.min(amount, count - start))

      {:error, module} ->
        slice_enum(enumerable, module, start, amount)
    end
  end

  defp slice_count_and_fun(enumerable) when is_list(enumerable) do
    {length(enumerable), &Enumerable.List.slice(enumerable, &1, &2)}
  end

  defp slice_count_and_fun(enumerable) do
    case Enumerable.slice(enumerable) do
      {:ok, count, fun} when is_function(fun) ->
        {count, fun}

      {:error, module} ->
        {_, {list, count}} =
          module.reduce(enumerable, {:cont, {[], 0}}, fn elem, {acc, count} ->
            {:cont, {[elem | acc], count + 1}}
          end)

        {count, &Enumerable.List.slice(:lists.reverse(list), &1, &2)}
    end
  end

end
