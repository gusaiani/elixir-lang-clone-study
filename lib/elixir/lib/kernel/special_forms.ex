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
  `alias/2` is used to set up aliases, often useful with modules' names.

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

  ## Selector

  By default, Elixir imports functions and macros from the given
  module, except the ones starting with underscore (which are
  usually callbacks):

      import List

  A developer can filter to import only macros or functions via
  the only option:

      import List, only: :functions
      import List, only: :macros

  Alternatively, Elixir allows a developer to pass pairs of
  name/arities to `:only` or `:except` as a fine grained control
  on what to import (or not):

      import List, only: [flatten: 1]
      import String, except: [split: 2]

  Notice that calling `except` is always exclusive on a previously
  declared `import/2`. If there is no previous import, then it applies
  to all functions and macros in the module. For example:

      import List, only: [flatten: 1, keyfind: 4]
      import List, except: [flatten: 1]

  After the two import calls above, only `List.keyfind/4` will be
  imported.

  ## Underscore functions

  By default functions starting with `_` are not imported. If you really want
  to import a function starting with `_` you must explicitly include it in the
  `:only` selector.

      import File.Stream, only: [__build__: 3]

  ## Lexical scope

  It is important to notice that `import/2` is lexical. This means you
  can import specific macros inside specific functions:

      defmodule Math do
        def some_function do
          # 1) Disable "if/2" from Kernel
          import Kernel, except: [if: 2]

          # 2) Require the new "if/2" macro from MyMacros
          import MyMacros

          # 3) Use the new macro
          if do_something, it_works
        end
      end

  In the example above, we imported macros from `MyMacros`,
  replacing the original `if/2` implementation by our own
  within that specific function. All other functions in that
  module will still be able to use the original one.

  ## Warnings

  If you import a module and you don't use any of the imported
  functions or macros from this module, Elixir is going to issue
  a warning implying the import is not being used.

  In case the import is generated automatically by a macro,
  Elixir won't emit any warnings though, since the import
  was not explicitly defined.

  Both warning behaviours could be changed by explicitly
  setting the `:warn` option to `true` or `false`.

  ## Ambiguous function/macro names

  If two modules `A` and `B` are imported and they both contain
  a `foo` function with an arity of `1`, an error is only emitted
  if an ambiguous call to `foo/1` is actually made; that is, the
  errors are emitted lazily, not eagerly.
  """
  defmacro import(module, opts), do: error!([module, opts])

  @doc """
  Returns the current environment information as a `Macro.Env` struct.

  In the environment you can access the current filename,
  line numbers, set up aliases, the current function and others.
  """
  defmacro __ENV__, do: error!([])

  @doc """
  Returns the current module name as an atom or `nil` otherwise.

  Although the module can be accessed in the `__ENV__/0`, this macro
  is a convenient shortcut.
  """
  defmacro __MODULE__, do: error!([])

  @doc """
  Returns the absolute path of the directory of the current file as a binary.

  Although the directory can be accessed as `Path.dirname(__ENV__.file)`,
  this macro is a convenient shortcut.
  """
  defmacro __DIR__, do: error!([])

  @doc """
  Returns the current calling environment as a `Macro.Env` struct.

  In the environment you can access the filename, line numbers,
  set up aliases, the function and others.
  """
  defmacro __CALLER__, do: error!([])

  @doc """
  Returns the stacktrace for the currently handled exception.

  It is available only in the `catch` and `rescue` clauses of `try/1`
  expressions.

  To retrieve the stacktrace of the current process, use
  `Process.info(self(), :current_stacktrace)` instead.
  """
  defmacro __STACKTRACE__, do: error!([])

  @doc """
  Accesses an already bound variable in match clauses. Also known as the pin operator.

  ## Examples

  Elixir allows variables to be rebound via static single assignment:

      iex> x = 1
      iex> x = x + 1
      iex> x
      2

  However, in some situations, it is useful to match against an existing
  value, instead of rebinding. This can be done with the `^` special form,
  colloquially known as the pin operator:

      iex> x = 1
      iex> ^x = List.first([1])
      iex> ^x = List.first([2])
      ** (MatchError) no match of right hand side value: 2

  Note that `^x` always refers to the value of `x` prior to the match. The
  following example will match:

      iex> x = 0
      iex> {x, ^x} = {1, 0}
      iex> x
      1

  """
  defmacro ^var, do: error!([var])

  @doc """
  Matches the value on the right against the pattern on the left.
  """
  defmacro left = right, do: error!([left, right])

  @doc """
  Used by types and bitstrings to specify types.

  This operator is used in two distinct occasions in Elixir.
  It is used in typespecs to specify the type of a variable,
  function or of a type itself:

      @type number :: integer | float
      @spec add(number, number) :: number

  It may also be used in bit strings to specify the type
  of a given bit segment:

      <<int::integer-little, rest::bits>> = bits

  Read the documentation on the `Typespec` page and
  `<<>>/1` for more information on typespecs and
  bitstrings respectively.
  """
  defmacro left :: right, do: error!([left, right])

  @doc ~S"""
  Gets the representation of any expression.

  ## Examples

      iex> quote do
      ...>   sum(1, 2, 3)
      ...> end
      {:sum, [], [1, 2, 3]}

  ## Elixir's AST (Abstract Syntax Tree)

  Any Elixir code can be represented using Elixir data structures.
  The building block of Elixir macros is a tuple with three elements,
  for example:

      {:sum, [], [1, 2, 3]}

  The tuple above represents a function call to `sum` passing 1, 2 and
  3 as arguments. The tuple elements are:

    * The first element of the tuple is always an atom or
      another tuple in the same representation.

    * The second element of the tuple represents metadata.

    * The third element of the tuple are the arguments for the
      function call. The third argument may be an atom, which is
      usually a variable (or a local call).

  Besides the tuple described above, Elixir has a few literals that
  are also part of its AST. Those literals return themselves when
  quoted. They are:

      :sum         #=> Atoms
      1            #=> Integers
      2.0          #=> Floats
      [1, 2]       #=> Lists
      "strings"    #=> Strings
      {key, value} #=> Tuples with two elements

  Any other value, such as a map or a four-element tuple, must be escaped
  (`Macro.escape/1`) before being introduced into an AST.

  ## Options

    * `:unquote` - when `false`, disables unquoting. This means any `unquote`
      call will be kept as is in the AST, instead of replaced by the `unquote`
      arguments. For example:

          iex> quote do
          ...>   unquote("hello")
          ...> end
          "hello"

          iex> quote unquote: false do
          ...>   unquote("hello")
          ...> end
          {:unquote, [], ["hello"]}

    * `:location` - when set to `:keep`, keeps the current line and file from
      quote. Read the Stacktrace information section below for more
      information.

    * `:line` - sets the quoted expressions to have the given line.

    * `:generated` - marks the given chunk as generated so it does not emit warnings.
      Currently it only works on special forms (for example, you can annotate a `case`
      but not an `if`).

    * `:context` - sets the resolution context.

    * `:bind_quoted` - passes a binding to the macro. Whenever a binding is
      given, `unquote/1` is automatically disabled.

  ## Quote and macros

  `quote/2` is commonly used with macros for code generation. As an exercise,
  let's define a macro that multiplies a number by itself (squared). In practice,
  there is no reason to define such a macro (and it would actually be
  seen as a bad practice), but it is simple enough that it allows us to focus
  on the important aspects of quotes and macros:

      defmodule Math do
        defmacro squared(x) do
          quote do
            unquote(x) * unquote(x)
          end
        end
      end

  We can invoke it as:

      import Math
      IO.puts "Got #{squared(5)}"

  At first, there is nothing in this example that actually reveals it is a
  macro. But what is happening is that, at compilation time, `squared(5)`
  becomes `5 * 5`. The argument `5` is duplicated in the produced code, we
  can see this behaviour in practice though because our macro actually has
  a bug:

      import Math
      my_number = fn ->
        IO.puts "Returning 5"
        5
      end
      IO.puts "Got #{squared(my_number.())}"

  The example above will print:

      Returning 5
      Returning 5
      Got 25

  Notice how "Returning 5" was printed twice, instead of just once. This is
  because a macro receives an expression and not a value (which is what we
  would expect in a regular function). This means that:

      squared(my_number.())

  Actually expands to:

      my_number.() * my_number.()

  Which invokes the function twice, explaining why we get the printed value
  twice! In the majority of the cases, this is actually unexpected behaviour,
  and that's why one of the first things you need to keep in mind when it
  comes to macros is to **not unquote the same value more than once**.

  Let's fix our macro:

      defmodule Math do
        defmacro squared(x) do
          quote do
            x = unquote(x)
            x * x
          end
        end
      end

  Now invoking `squared(my_number.())` as before will print the value just
  once.

  In fact, this pattern is so common that most of the times you will want
  to use the `bind_quoted` option with `quote/2`:

      defmodule Math do
        defmacro squared(x) do
          quote bind_quoted: [x: x] do
            x * x
          end
        end
      end

  `:bind_quoted` will translate to the same code as the example above.
  `:bind_quoted` can be used in many cases and is seen as good practice,
  not only because it helps prevent us from running into common mistakes, but also
  because it allows us to leverage other tools exposed by macros, such as
  unquote fragments discussed in some sections below.

  Before we finish this brief introduction, you will notice that, even though
  we defined a variable `x` inside our quote:

      quote do
        x = unquote(x)
        x * x
      end

  When we call:

      import Math
      squared(5)
      x #=> ** (CompileError) undefined variable x or undefined function x/0

  We can see that `x` did not leak to the user context. This happens
  because Elixir macros are hygienic, a topic we will discuss at length
  in the next sections as well.

  ## Hygiene in variables

  Consider the following example:

      defmodule Hygiene do
        defmacro no_interference do
          quote do
            a = 1
          end
        end
      end

      require Hygiene

      a = 10
      Hygiene.no_interference
      a #=> 10

  In the example above, `a` returns 10 even if the macro
  is apparently setting it to 1 because variables defined
  in the macro do not affect the context the macro is executed in.
  If you want to set or get a variable in the caller's context, you
  can do it with the help of the `var!` macro:

      defmodule NoHygiene do
        defmacro interference do
          quote do
            var!(a) = 1
          end
        end
      end

      require NoHygiene

      a = 10
      NoHygiene.interference
      a #=> 1

  You cannot even access variables defined in the same module unless
  you explicitly give it a context:

      defmodule Hygiene do
        defmacro write do
          quote do
            a = 1
          end
        end

        defmacro read do
          quote do
            a
          end
        end
      end

      Hygiene.write
      Hygiene.read
      #=> ** (RuntimeError) undefined variable a or undefined function a/0

  For such, you can explicitly pass the current module scope as
  argument:

      defmodule ContextHygiene do
        defmacro write do
          quote do
            var!(a, ContextHygiene) = 1
          end
        end

        defmacro read do
          quote do
            var!(a, ContextHygiene)
          end
        end
      end

      ContextHygiene.write
      ContextHygiene.read
      #=> 1

  ## Hygiene in aliases

  Aliases inside quote are hygienic by default.
  Consider the following example:

      defmodule Hygiene do
        alias Map, as: M

        defmacro no_interference do
          quote do
            M.new
          end
        end
      end

      require Hygiene
      Hygiene.no_interference #=> %{}

  Notice that, even though the alias `M` is not available
  in the context the macro is expanded, the code above works
  because `M` still expands to `Map`.

  Similarly, even if we defined an alias with the same name
  before invoking a macro, it won't affect the macro's result:

      defmodule Hygiene do
        alias Map, as: M

        defmacro no_interference do
          quote do
            M.new
          end
        end
      end

      require Hygiene
      alias SomethingElse, as: M
      Hygiene.no_interference #=> %{}

  In some cases, you want to access an alias or a module defined
  in the caller. For such, you can use the `alias!` macro:

      defmodule Hygiene do
        # This will expand to Elixir.Nested.hello
        defmacro no_interference do
          quote do
            Nested.hello
          end
        end

        # This will expand to Nested.hello for
        # whatever is Nested in the caller
        defmacro interference do
          quote do
            alias!(Nested).hello
          end
        end
      end

      defmodule Parent do
        defmodule Nested do
          def hello, do: "world"
        end

        require Hygiene
        Hygiene.no_interference
        #=> ** (UndefinedFunctionError) ...

        Hygiene.interference
        #=> "world"
      end

  ## Hygiene in imports

  Similar to aliases, imports in Elixir are hygienic. Consider the
  following code:

      defmodule Hygiene do
        defmacrop get_length do
          quote do
            length([1, 2, 3])
          end
        end

        def return_length do
          import Kernel, except: [length: 1]
          get_length
        end
      end

      Hygiene.return_length #=> 3

  Notice how `Hygiene.return_length/0` returns `3` even though the `Kernel.length/1`
  function is not imported. In fact, even if `return_length/0`
  imported a function with the same name and arity from another
  module, it wouldn't affect the function result:

      def return_length do
        import String, only: [length: 1]
        get_length
      end

  Calling this new `return_length/0` will still return `3` as result.

  Elixir is smart enough to delay the resolution to the latest
  possible moment. So, if you call `length([1, 2, 3])` inside quote,
  but no `length/1` function is available, it is then expanded in
  the caller:

      defmodule Lazy do
        defmacrop get_length do
          import Kernel, except: [length: 1]

          quote do
            length("hello")
          end
        end

        def return_length do
          import Kernel, except: [length: 1]
          import String, only: [length: 1]
          get_length
        end
      end

      Lazy.return_length #=> 5

  ## Stacktrace information

  When defining functions via macros, developers have the option of
  choosing if runtime errors will be reported from the caller or from
  inside the quote. Let's see an example:

      # adder.ex
      defmodule Adder do
        @doc "Defines a function that adds two numbers"
        defmacro defadd do
          quote location: :keep do
            def add(a, b), do: a + b
          end
        end
      end

      # sample.ex
      defmodule Sample do
        import Adder
        defadd
      end

      require Sample
      Sample.add(:one, :two)
      #=> ** (ArithmeticError) bad argument in arithmetic expression
      #=>     adder.ex:5: Sample.add/2

end
