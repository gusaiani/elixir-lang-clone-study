# Typespecs

Elixir comes with a notation for declaring types and specifications. Elixir is a dynamically typed language, and as such, type specifications are never used by the compiler to optimize or modify code. Still, using type specifications is useful because

  * they provide documentation (for example, tools such as [ExDoc](https://github.com/elixir-lang/ex_doc) show type specifications in the documentation)
  * they're used by tools such as [Dialyzer](http://www.erlang.org/doc/man/dialyzer.html), that can analyze code with typespec to find type inconsistencies and possible bugs

Type specifications (sometimes referred to as *typespecs*) are defined in different contexts using the following attributes:

  * `@type`
  * `@opaque`
  * `@typep`
  * `@spec`
  * `@callback`
  * `@macrocallback`

See the "User-defined types" and "Defining a specification" sub-sections below for more information on defining types and typespecs.

## A simple example

    defmodule StringHelpers do
      @type word() :: String.t()

      @spec long_word?(word()) :: boolean()
      def long_word?(word) when is_binary(word) do
        String.length(word) > 8
      end
    end

In the example above, this happens:

  * we declare a new type (`word()`) that is equivalent to the string type (`String.t()`);

  * we specify that the `long_word?/1` function takes an argument of type `word()` and
    returns a boolean (`boolean()`), that is, either `true` or `false`.

## Types and their syntax

The syntax Elixir provides for type specifications is similar to [the one in Erlang](http://www.erlang.org/doc/reference_manual/typespec.html). Most of the built-in types provided in Erlang (for example, `pid()`) are expressed in the same way: `pid()` (or simply `pid`). Parameterized types (such as `list(integer)`) are supported as well and so are remote types (such as `Enum.t`). Integers and atom literals are allowed as types (e.g., `1`, `:atom`, or `false`). All other types are built out of unions of predefined types. Some shorthands are allowed, such as `[...]`, `<<>>`, and `{...}`.

The notation to represent the union of types is the pipe `|`. For example, the typespec `type :: atom() | pid() | tuple()` creates a type `type` that can be either an `atom`, a `pid`, or a `tuple`. This is usually called a [sum type](https://en.wikipedia.org/wiki/Tagged_union) in other languages

### Basic types

    type ::
          any()                     # the top type, the set of all terms
          | none()                  # the bottom type, contains no terms
          | atom()
          | map()                   # any map
          | pid()                   # process identifier
          | port()                  # port identifier
          | reference()
          | struct()                # any struct
          | tuple()                 # tuple of any size

                                    ## Numbers
          | float()
          | integer()
          | neg_integer()           # ..., -3, -2, -1
          | non_neg_integer()       # 0, 1, 2, 3, ...
          | pos_integer()           # 1, 2, 3, ...

                                                        ## Lists
          | list(type)                                  # proper list ([]-terminated)
          | nonempty_list(type)                         # non-empty proper list
          | maybe_improper_list(type1, type2)           # proper or improper list
          | nonempty_improper_list(type1, type2)        # improper list
          | nonempty_maybe_improper_list(type1, type2)  # non-empty proper or improper list

          | Literals                # Described in section "Literals"
          | BuiltIn                 # Described in section "Built-in types"
          | Remotes                 # Described in section "Remote types"
          | UserDefined             # Described in section "User-defined types"

### Literals

The following literals are also supported in typespecs:

    type ::                               ## Atoms
          :atom                           # atoms: :foo, :bar, ...
          | true | false | nil            # special atom literals

                                          ## Bitstrings
          | <<>>                          # empty bitstring
          | <<_::size>>                   # size is 0 or a positive integer
          | <<_::_*unit>>                 # unit is an integer from 1 to 256
          | <<_::size, _::_*unit>>

                                          ## (Anonymous) Functions
          | (-> type)                     # 0-arity, returns type
          | (type1, type2 -> type)        # 2-arity, returns type
          | (... -> type)                 # any arity, returns type

                                          ## Integers
          | 1                             # integer
          | 1..10                         # integer from 1 to 10

                                          ## Lists
          | [type]                        # list with any number of type elements
          | []                            # empty list
          | [...]                         # shorthand for nonempty_list(any())
          | [type, ...]                   # shorthand for nonempty_list(type)
          | [key: value_type]             # keyword list with key :key of value_type

                                                  ## Maps
          | %{}                                   # empty map
          | %{key: value_type}                    # map with required key :key of value_type
          | %{required(key_type) => value_type}   # map with required pairs of key_type and value_type
          | %{optional(key_type) => value_type}   # map with optional pairs of key_type and value_type
          | %SomeStruct{}                         # struct with all fields of any type
          | %SomeStruct{key: value_type}          # struct with required key :key of value_type

                                          ## Tuples
          | {}                            # empty tuple
          | {:ok, type}                   # two-element tuple with an atom and any type
