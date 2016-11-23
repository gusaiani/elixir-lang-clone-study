defmodule List do
  @moduledoc """
  Functions that work on (linked) lists.

  Lists in Elixir are specified between square brackets:

      iex> [1, "two", 3, :four]
      [1, "two", 3, :four]

  Two lists can be concatenated and subtracted using the
  `Kernel.++/2` and `Kernel.--/2` operators:

      iex> [1, 2, 3] ++ [4, 5, 6]
      [1, 2, 3, 4, 5, 6]
      iex> [1, true, 2, false, 3, true] -- [true, false]
      [1, 2, 3, true]

  Lists in Elixir are effectively linked lists, which means
  they are internally represented in pairs containing the
  head and the tail of a list:

      iex> [head | tail] = [1, 2, 3]
      iex> head
      1
      iex> tail
      [2, 3]

  Similarly, we could write the list `[1, 2, 3]` using only
  such pairs (called cons cells):

      iex> [1 | [2 | [3 | []]]]
      [1, 2, 3]

  Some lists, called improper lists, do not have an empty list as
  the second element in the last cons cell:

      iex> [1 | [2 | [3 | 4]]]
      [1, 2, 3 | 4]

  Although improper lists are generally avoided, they are used in some
  special circumstances like iodata and chardata entities (see the `IO` module).

  Due to their cons cell based representation, prepending an element
  to a list is always fast (constant time), while appending becomes
  slower as the list grows in size (linear time):

      iex> list = [1, 2, 3]
      iex> [0 | list]   # fast
      [0, 1, 2, 3]
      iex> list ++ [4]  # slow
      [1, 2, 3, 4]

  The `Kernel` module contains many functions to manipulate lists
  and that are allowed in guards. For example, `Kernel.hd/1` to
  retrieve the head, `Kernel.tl/1` to fetch the tail and
  `Kernel.length/1` for calculating the length. Keep in mind that,
  similar to appending to a list, calculating the length needs to
  traverse the whole list.

  ## Charlists

  If a list is made of non-negative integers, it can also be called
  a charlist. Elixir uses single quotes to define charlists:

      iex> 'héllo'
      [104, 233, 108, 108, 111]

  In particular, charlists may be printed back in single
  quotes if they contain only ASCII-printable codepoints:

      iex> 'abc'
      'abc'

  The rationale behind this behaviour is to better support
  Erlang libraries which may return text as charlists
  instead of Elixir strings. One example of such functions
  is `Application.loaded_applications/0`:

      Application.loaded_applications
      #=>  [{:stdlib, 'ERTS  CXC 138 10', '2.6'},
            {:compiler, 'ERTS  CXC 138 10', '6.0.1'},
            {:elixir, 'elixir', '1.0.0'},
            {:kernel, 'ERTS  CXC 138 10', '4.1'},
            {:logger, 'logger', '1.0.0'}]

  ## List and Enum modules

  This module aims to provide operations that are specific
  to lists, like conversion between data types, updates,
  deletions and key lookups (for lists of tuples). For traversing
  lists in general, developers should use the functions in the
  `Enum` module that work across a variety of data types.

  In both `Enum` and `List` modules, any kind of index access
  on a list is linear. Negative indexes are also supported but
  they imply the list will be iterated twice, one to calculate
  the proper index and another to perform the operation.
  """

  @compile :inline_list_funcs

  @doc """
  Deletes the given `item` from the `list`. Returns a new list without
  the item.

  If the `item` occurs more than once in the `list`, just
  the first occurrence is removed.

  ## Examples

      iex> List.delete([:a, :b, :c], :a)
      [:b, :c]

      iex> List.delete([:a, :b, :b, :c], :b)
      [:a, :b, :c]

  """
  @spec delete(list, any) :: list
  def delete(list, item) do
    :lists.delete(item, list)
  end

  @doc """
  Duplicates the given element `n` times in a list.

  ## Examples

      iex> List.duplicate("hello", 3)
      ["hello", "hello", "hello"]

      iex> List.duplicate([1, 2], 2)
      [[1, 2], [1, 2]]

  """
  @spec duplicate(elem, non_neg_integer) :: [elem] when elem: var
  def duplicate(elem, n) do
    :lists.duplicate(n, elem)
  end

  @doc """
  Flattens the given `list` of nested lists.

  ## Examples

      iex> List.flatten([1, [[2], 3]])
      [1, 2, 3]

  """
  @spec flatten(deep_list) :: list when deep_list: [any | deep_list]
  def flatten(list) do
    :lists.flatten(list)
  end

  @doc """
  Flattens the given `list` of nested lists.
  The list `tail` will be added at the end of
  the flattened list.

  ## Examples

      iex> List.flatten([1, [[2], 3]], [4, 5])
      [1, 2, 3, 4, 5]

  """
  @spec flatten(deep_list, [elem]) :: [elem] when elem: var, deep_list: [elem | deep_list]
  def flatten(list, tail) do
    :lists.flatten(list, tail)
  end

  @doc """
  Folds (reduces) the given list from the left with
  a function. Requires an accumulator.

  ## Examples

      iex> List.foldl([5, 5], 10, fn(x, acc) -> x + acc end)
      20

      iex> List.foldl([1, 2, 3, 4], 0, fn(x, acc) -> x - acc end)
      2

  """
  @spec foldl([elem], acc, (elem, acc -> acc)) :: acc when elem: var, acc: var
  def foldl(list, acc, function) when is_list(list) and is_function(function) do
    :lists.foldl(function, acc, list)
  end
end
