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

  """
end
