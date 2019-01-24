defmodule Kernel.SpecialForms do
  @moduledoc """
  Special forms are the basic building blocks of Elixir, and therefore
  cannot be overridden by the developer.

  We define them in this module. Some of these forms are lexical (like
  `alias/2`, `case/2`, etc.). The macros `{}/1` and `<<>>/1` are also special
  forms used to define tuple and binary data structures respectively.

  This module also documents macros that return information about Elixir's
  compilation environment, such as (`__ENV__/0`, `__MODULE__/0`, `__DIR__/0` and `__CALLER__/0`).

  Finally, it also documents two special forms, `__block__/1` and
  `__aliases__/1`, which are not intended to be called directly by the
  developer but they appear in quoted contents since they are essential
  in Elixir's constructs.
  """

  defmacrop error!(args) do
    quote do
      _ = unquote(args)

      message =
        "Elixir's special forms are expanded by the compiler and must not be invoked directly"

      :erlang.error(RuntimeError.exception(message))
    end
  end

  @doc """
  Creates a tuple.

  More information about the tuple data type and about functions to manipulate
  tuples can be found in the `Tuple` module; some functions for working with
  tuples are also available in `Kernel` (such as `Kernel.elem/2` or
  `Kernel.tuple_size/1`).

  ## AST representation

  Only two-item tuples are considered literals in Elixir and return themselves
  when quoted. Therefore, all other tuples are represented in the AST as calls to
  the `:{}` special form.

      iex> quote do
      ...>   {1, 2}
      ...> end
      {1, 2}

      iex> quote do
      ...>   {1, 2, 3}
      ...> end
      {:{}, [], [1, 2, 3]}

  """
  defmacro unquote(:{})(args), do: error!([args])

  @doc """
  Creates a map.

  See the `Map` module for more information about maps, their syntax, and ways to
  access and manipulate them.

  ## AST representation

  Regardless of whether `=>` or the keyword syntax is used, key-value pairs in
  maps are always represented internally as a list of two-element tuples for
  simplicity:

      iex> quote do
      ...>   %{"a" => :b, c: :d}
      ...> end
      {:%{}, [], [{"a", :b}, {:c, :d}]}

  """
  defmacro unquote(:%{})(args), do: error!([args])

  @doc """
  Matches on or builds a struct.

  A struct is a tagged map that allows developers to provide
  default values for keys, tags to be used in polymorphic
  dispatches and compile time assertions.

  Structs are usually defined with the `Kernel.defstruct/1` macro:

      defmodule User do
        defstruct name: "john", age: 27
      end

  Now a struct can be created as follows:

      %User{}

  Underneath a struct is just a map with a `:__struct__` key
  pointing to the `User` module:

      %User{} == %{__struct__: User, name: "john", age: 27}

  The struct fields can be given when building the struct:

      %User{age: 31}
      #=> %{__struct__: User, name: "john", age: 31}

  Or also on pattern matching to extract values out:

      %User{age: age} = user

  An update operation specific for structs is also available:

      %User{user | age: 28}

  The advantage of structs is that they validate that the given
  keys are part of the defined struct. The example below will fail
  because there is no key `:full_name` in the `User` struct:

      %User{full_name: "john doe"}

  The syntax above will guarantee the given keys are valid at
  compilation time and it will guarantee at runtime the given
  argument is a struct, failing with `BadStructError` otherwise.

  Although structs are maps, by default structs do not implement
  any of the protocols implemented for maps. Check
  `Kernel.defprotocol/2` for more information on how structs
  can be used with protocols for polymorphic dispatch. Also
  see `Kernel.struct/2` and `Kernel.struct!/2` for examples on
  how to create and update structs dynamically.

  ## Pattern matching on struct names

  Besides allowing pattern matching on struct fields, such as:

      %User{age: age} = user

  Structs also allow pattern matching on the struct name:

      %struct_name{} = user
      struct_name #=> User

  You can also assign the struct name to `_` when you want to
  check if something is a struct but you are not interested in
  its name:

      %_{} = user

  """
  defmacro unquote(:%)(struct, map), do: error!([struct, map])

  @doc """
  Defines a new bitstring.

  ## Examples

      iex> <<1, 2, 3>>
      <<1, 2, 3>>

  ## Types

  A bitstring is made of many segments and each segment has a
  type. There are 9 types used in bitstrings:

  - `integer`
  - `float`
  - `bits` (alias for `bitstring`)
  - `bitstring`
  - `binary`
  - `bytes` (alias for `binary`)
  - `utf8`
  - `utf16`
  - `utf32`

  When no type is specified, the default is `integer`:

      iex> <<1, 2, 3>>
      <<1, 2, 3>>

  Elixir also accepts by default the segment to be a literal
  string or a literal charlist, which are by default expanded to integers:

      iex> <<0, "foo">>
      <<0, 102, 111, 111>>

  Variables or any other type need to be explicitly tagged:

      iex> rest = "oo"
      iex> <<102, rest>>
      ** (ArgumentError) argument error

  We can solve this by explicitly tagging it as `binary`:

      iex> rest = "oo"
      iex> <<102, rest::binary>>
      "foo"

  The `utf8`, `utf16`, and `utf32` types are for Unicode codepoints. They
  can also be applied to literal strings and charlists:

      iex> <<"foo"::utf16>>
      <<0, 102, 0, 111, 0, 111>>
      iex> <<"foo"::utf32>>
      <<0, 0, 0, 102, 0, 0, 0, 111, 0, 0, 0, 111>>

  ## Options

  Many options can be given by using `-` as separator. Order is
  arbitrary, so the following are all equivalent:

      <<102::integer-native, rest::binary>>
      <<102::native-integer, rest::binary>>
      <<102::unsigned-big-integer, rest::binary>>
      <<102::unsigned-big-integer-size(8), rest::binary>>
      <<102::unsigned-big-integer-8, rest::binary>>
      <<102::8-integer-big-unsigned, rest::binary>>
      <<102, rest::binary>>

  ### Unit and Size

  The length of the match is equal to the `unit` (a number of bits) times the
  `size` (the number of repeated segments of length `unit`).

  Type      | Default Unit
  --------- | ------------
  `integer` | 1 bit
  `float`   | 1 bit
  `binary`  | 8 bits

  Sizes for types are a bit more nuanced. The default size for integers is 8.

  For floats, it is 64. For floats, `size * unit` must result in 32 or 64,
  corresponding to [IEEE 754](https://en.wikipedia.org/wiki/IEEE_floating_point)
  binary32 and binary64, respectively.

  For binaries, the default is the size of the binary. Only the last binary in a
  match can use the default size. All others must have their size specified
  explicitly, even if the match is unambiguous. For example:

      iex> <<name::binary-size(5), " the ", species::binary>> = <<"Frank the Walrus">>
      "Frank the Walrus"
      iex> {name, species}
      {"Frank", "Walrus"}

  The size can be a variable:

      iex> name_size = 5
      iex> <<name::binary-size(name_size), " the ", species::binary>> = <<"Frank the Walrus">>
      iex> {name, species}
      {"Frank", "Walrus"}

  And the variable can be defined in the match itself (prior to its use):

      iex> <<name_size::size(8), name::binary-size(name_size), " the ", species::binary>> = <<5, "Frank the Walrus">>
      iex> {name, species}
      {"Frank", "Walrus"}
  """
end
