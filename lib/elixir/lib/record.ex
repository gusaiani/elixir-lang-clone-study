defmodule Record do
  @moduledoc """
  Module to work with, define, and import records.

  Records are simply tuples where the first element is an atom:

      iex> Record.is_record({User, "john", 27})
      true

  This module provides conveniences for working with records at
  compilation time, where compile-time field names are used to
  manipulate the tuples, providing fast operations on top of
  the tuples' compact structure.

  In Elixir, records are used mostly in two situations:

    1. to work with short, internal data
    2. to interface with Erlang records

  The macros `defrecord/3` and `defrecordp/3` can be used to create records
  while `extract/2` and `extract_all/1` can be used to extract records from
  Erlang files.

  ## Types

  Types can be defined for tuples with the `record/2` macro (only available in
  typespecs). This macro will expand to a tuple as seen in the example below:

      defmodule MyModule do
        require Record
        Record.defrecord(:user, name: "john", age: 25)

        @type user :: record(:user, name: String.t(), age: integer)
        # expands to: "@type user :: {:user, String.t(), integer}"
      end

  """
end
