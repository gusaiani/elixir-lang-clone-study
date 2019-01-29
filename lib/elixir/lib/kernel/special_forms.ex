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

  However, the size cannot be defined in the match outside the binary/bitstring match:

      {name_size, <<name::binary-size(name_size), _rest::binary>>} = {5, <<"Frank the Walrus">>}
      ** (CompileError): undefined variable "name_size" in bitstring segment

  Failing to specify the size for the non-last causes compilation to fail:

      <<name::binary, " the ", species::binary>> = <<"Frank the Walrus">>
      ** (CompileError): a binary field without size is only allowed at the end of a binary pattern

  #### Shortcut Syntax

  Size and unit can also be specified using a syntax shortcut
  when passing integer values:

      iex> x = 1
      iex> <<x::8>> == <<x::size(8)>>
      true
      iex> <<x::8*4>> == <<x::size(8)-unit(4)>>
      true

  This syntax reflects the fact the effective size is given by
  multiplying the size by the unit.

  ### Modifiers

  Some types have associated modifiers to clear up ambiguity in byte
  representation.

  Modifier             | Relevant Type(s)
  -------------------- | ----------------
  `signed`             | `integer`
  `unsigned` (default) | `integer`
  `little`             | `integer`, `float`, `utf16`, `utf32`
  `big` (default)      | `integer`, `float`, `utf16`, `utf32`
  `native`             | `integer`, `utf16`, `utf32`

  ### Sign

  Integers can be `signed` or `unsigned`, defaulting to `unsigned`.

      iex> <<int::integer>> = <<-100>>
      <<156>>
      iex> int
      156
      iex> <<int::integer-signed>> = <<-100>>
      <<156>>
      iex> int
      -100

  `signed` and `unsigned` are only used for matching binaries (see below) and
  are only used for integers.

      iex> <<-100::signed, _rest::binary>> = <<-100, "foo">>
      <<156, 102, 111, 111>>

  ### Endianness

  Elixir has three options for endianness: `big`, `little`, and `native`.
  The default is `big`:

      iex> <<number::little-integer-size(16)>> = <<0, 1>>
      <<0, 1>>
      iex> number
      256
      iex> <<number::big-integer-size(16)>> = <<0, 1>>
      <<0, 1>>
      iex> number
      1

  `native` is determined by the VM at startup and will depend on the
  host operating system.

  ## Binary/Bitstring Matching

  Binary matching is a powerful feature in Elixir that is useful for extracting
  information from binaries as well as pattern matching.

  Binary matching can be used by itself to extract information from binaries:

      iex> <<"Hello, ", place::binary>> = "Hello, World"
      "Hello, World"
      iex> place
      "World"

  Or as a part of function definitions to pattern match:

      defmodule ImageTyper do
        @png_signature <<137::size(8), 80::size(8), 78::size(8), 71::size(8),
                         13::size(8), 10::size(8), 26::size(8), 10::size(8)>>
        @jpg_signature <<255::size(8), 216::size(8)>>

        def type(<<@png_signature, rest::binary>>), do: :png
        def type(<<@jpg_signature, rest::binary>>), do: :jpg
        def type(_), do: :unknown
      end

  ### Performance & Optimizations

  The Erlang compiler can provide a number of optimizations on binary creation
  and matching. To see optimization output, set the `bin_opt_info` compiler
  option:

      ERL_COMPILER_OPTIONS=bin_opt_info mix compile

  To learn more about specific optimizations and performance considerations,
  check out
  [Erlang's Efficiency Guide on handling binaries](http://www.erlang.org/doc/efficiency_guide/binaryhandling.html).
  """
  defmacro unquote(:<<>>)(args), do: error!([args])

  @doc """
  Defines a remote call, a call to an anonymous function, or an alias.

  The dot (`.`) in Elixir can be used for remote calls:

      iex> String.downcase("FOO")
      "foo"

  In this example above, we have used `.` to invoke `downcase` in the
  `String` module, passing `"FOO"` as argument.

  The dot may be used to invoke anonymous functions too:

      iex> (fn n -> n end).(7)
      7

  in which case there is a function on the left hand side.

  We can also use the dot for creating aliases:

      iex> Hello.World
      Hello.World

  This time, we have joined two aliases, defining the final alias
  `Hello.World`.

  ## Syntax

  The right side of `.` may be a word starting with an uppercase letter, which represents
  an alias, a word starting with lowercase or underscore, any valid language
  operator or any name wrapped in single- or double-quotes. Those are all valid
  examples:

      iex> Kernel.Sample
      Kernel.Sample

      iex> Kernel.length([1, 2, 3])
      3

      iex> Kernel.+(1, 2)
      3

      iex> Kernel."+"(1, 2)
      3

  Wrapping the function name in single- or double-quotes is always a
  remote call. Therefore `Kernel."Foo"` will attempt to call the function "Foo"
  and not return the alias `Kernel.Foo`. This is done by design as module names
  are more strict than function names.

  When the dot is used to invoke an anonymous function there is only one
  operand, but it is still written using a postfix notation:

      iex> negate = fn n -> -n end
      iex> negate.(7)
      -7

  ## Quoted expression

  When `.` is used, the quoted expression may take two distinct
  forms. When the right side starts with a lowercase letter (or
  underscore):

      iex> quote do
      ...>   String.downcase("FOO")
      ...> end
      {{:., [], [{:__aliases__, [alias: false], [:String]}, :downcase]}, [], ["FOO"]}

  Notice we have an inner tuple, containing the atom `:.` representing
  the dot as first element:

      {:., [], [{:__aliases__, [alias: false], [:String]}, :downcase]}

  This tuple follows the general quoted expression structure in Elixir,
  with the name as first argument, some keyword list as metadata as second,
  and the list of arguments as third. In this case, the arguments are the
  alias `String` and the atom `:downcase`. The second argument in a remote call
  is **always** an atom.

  In the case of calls to anonymous functions, the inner tuple with the dot
  special form has only one argument, reflecting the fact that the operator is
  unary:

      iex> quote do
      ...>   negate.(0)
      ...> end
      {{:., [], [{:negate, [], __MODULE__}]}, [], [0]}

  When the right side is an alias (i.e. starts with uppercase), we get instead:

      iex> quote do
      ...>   Hello.World
      ...> end
      {:__aliases__, [alias: false], [:Hello, :World]}

  We go into more details about aliases in the `__aliases__/1` special form
  documentation.

  ## Unquoting

  We can also use unquote to generate a remote call in a quoted expression:

      iex> x = :downcase
      iex> quote do
      ...>   String.unquote(x)("FOO")
      ...> end
      {{:., [], [{:__aliases__, [alias: false], [:String]}, :downcase]}, [], ["FOO"]}

  Similar to `Kernel."FUNCTION_NAME"`, `unquote(x)` will always generate a remote call,
  independent of the value of `x`. To generate an alias via the quoted expression,
  one needs to rely on `Module.concat/2`:

      iex> x = Sample
      iex> quote do
      ...>   Module.concat(String, unquote(x))
      ...> end
      {{:., [], [{:__aliases__, [alias: false], [:Module]}, :concat]}, [],
       [{:__aliases__, [alias: false], [:String]}, Sample]}

  """
  defmacro unquote(:.)(left, right), do: error!([left, right])

  @doc """
  `alias/2` is used to setup aliases, often useful with modules' names.

  ## Examples

  `alias/2` can be used to set up an alias for any module:

      defmodule Math do
        alias MyKeyword, as: Keyword
      end

  In the example above, we have set up `MyKeyword` to be aliased
  as `Keyword`. So now, any reference to `Keyword` will be
  automatically replaced by `MyKeyword`.

  In case one wants to access the original `Keyword`, it can be done
  by accessing `Elixir`:

      Keyword.values #=> uses MyKeyword.values
      Elixir.Keyword.values #=> uses Keyword.values

  Notice that calling `alias` without the `:as` option automatically
  sets an alias based on the last part of the module. For example:

      alias Foo.Bar.Baz

  Is the same as:

      alias Foo.Bar.Baz, as: Baz

  We can also alias multiple modules in one line:

      alias Foo.{Bar, Baz, Biz}

  Is the same as:

      alias Foo.Bar
      alias Foo.Baz
      alias Foo.Biz

  ## Lexical scope

  `import/2`, `require/2` and `alias/2` are called directives and all
  have lexical scope. This means you can set up aliases inside
  specific functions and it won't affect the overall scope.

  ## Warnings

  If you alias a module and you don't use the alias, Elixir is
  going to issue a warning implying the alias is not being used.

  In case the alias is generated automatically by a macro,
  Elixir won't emit any warnings though, since the alias
  was not explicitly defined.

  Both warning behaviours could be changed by explicitly
  setting the `:warn` option to `true` or `false`.

  """
  defmacro alias(module, opts), do: error!([module, opts])

  @doc """
  Requires a module in order to use its macros.

  ## Examples

  Public functions in modules are globally available, but in order to use
  macros, you need to opt-in by requiring the module they are defined in.

  Let's suppose you created your own `if/2` implementation in the module
  `MyMacros`. If you want to invoke it, you need to first explicitly
  require the `MyMacros`:

      defmodule Math do
        require MyMacros
        MyMacros.if do_something, it_works
      end

  An attempt to call a macro that was not loaded will raise an error.

  ## Alias shortcut

  `require/2` also accepts `:as` as an option so it automatically sets
  up an alias. Please check `alias/2` for more information.

  """
  defmacro require(module, opts), do: error!([module, opts])

  @doc """
  Imports functions and macros from other modules.

  `import/2` allows one to easily access functions or macros from
  other modules without using the qualified name.

  ## Examples

  If you are using several functions from a given module, you can
  import those functions and reference them as local functions,
  for example:

      iex> import List
      iex> flatten([1, [2], 3])
      [1, 2, 3]
  """
end
