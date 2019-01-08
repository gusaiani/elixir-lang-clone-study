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

The syntax Elixir provides for type specifications is similar to [the one in Erlang](http://www.erlang.org/doc/reference_manual/typespec.html). Most of the build-in types provided in Erlang (for example, `pid()`) are expressed in the same way: `pid()` (or simply `pid`).

