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

  @doc """
  Extracts record information from an Erlang file.

  Returns a quoted expression containing the fields as a list
  of tuples.

  `name`, which is the name of the extracted record, is expected to be an atom
  *at compile time*.

  ## Options

  This function accepts the following options, which are exclusive to each other
  (i.e., only one of them can be used in the same call):

    * `:from` - (binary representing a path to a file) path to the Erlang file
      that contains the record definition to extract; with this option, this
      function uses the same path lookup used by the `-include` attribute used in
      Erlang modules.

    * `:from_lib` - (binary representing a path to a file) path to the Erlang
      file that contains the record definition to extract; with this option,
      this function uses the same path lookup used by the `-include_lib`
      attribute used in Erlang modules.

    * `:includes` - (a list of directories as binaries) if the record being
      extracted depends on relative includes, this option allows developers
      to specify the directory those relative includes exist.

    * `:macros` - (keyword list of macro names and values) if the record
      being extracted depends on the values of macros, this option allows
      the value of those macros to be set.

  These options are expected to be literals (including the binary values) at
  compile time.

  ## Examples

      iex> Record.extract(:file_info, from_lib: "kernel/include/file.hrl")
      [
        size: :undefined,
        type: :undefined,
        access: :undefined,
        atime: :undefined,
        mtime: :undefined,
        ctime: :undefined,
        mode: :undefined,
        links: :undefined,
        major_device: :undefined,
        minor_device: :undefined,
        inode: :undefined,
        uid: :undefined,
        gid: :undefined
      ]

  """
  @spec extract(name :: atom, keyword) :: keyword
  def extract(name, opts) when is_atom(name) and is_list(opts) do
    Record.Extractor.extract(name, opts)
  end

  @doc """
  Extracts all records information from an Erlang file.

  Returns a keyword list of `{record_name, fields}` tuples where `record_name`
  is the name of an extracted record and `fields` is a list of `{field, value}`
  tuples representing the fields for that record.

  ## Options

  This function accepts the following options, which are exclusive to each other
  (i.e., only one of them can be used in the same call):

    * `:from` - (binary representing a path to a file) path to the Erlang file
      that contains the record definitions to extract; with this option, this
      function uses the same path lookup used by the `-include` attribute used in
      Erlang modules.

    * `:from_lib` - (binary representing a path to a file) path to the Erlang
      file that contains the record definitions to extract; with this option,
      this function uses the same path lookup used by the `-include_lib`
      attribute used in Erlang modules.

  These options are expected to be literals (including the binary values) at
  compile time.
  """
  @spec extract_all(keyword) :: [{name :: atom, keyword}]
  def extract_all(opts) when is_list(opts) do
  end
end
