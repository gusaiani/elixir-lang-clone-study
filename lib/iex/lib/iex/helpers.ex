defmodule IEx.Helpers do
  @moduledoc """
  Welcome to Interactive Elixir. You are currently
  seeing the documentation for the module `IEx.Helpers`
  which provides many helpers to make Elixir's shell
  more joyful to work with.

  This message was triggered by invoking the helper `h()`,
  usually referred to as `h/0` (since it expects 0 arguments).

  You can use the `h/1` function to invoke the documentation
  for any Elixir module or function:

      iex> h(Enum)
      iex> h(Enum.map)
      iex> h(Enum.reverse/1)

  You can also use the `i/1` function to introspect any value
  you have in the shell:

      iex> i("hello")

  There are many other helpers available, here are some examples:

    * `b/1`            - prints callbacks info and docs for a given module
    * `c/1`            - compiles a file
    * `c/2`            - compiles a file and writes bytecode to the given path
    * `cd/1`           - changes the current directory
    * `clear/0`        - clears the screen
    * `exports/1`      - shows all exports (functions + macros) in a module
    * `flush/0`        - flushes all messages sent to the shell
    * `h/0`            - prints this help message
    * `h/1`            - prints help for the given module, function or macro
    * `i/0`            - prints information about the last value
    * `i/1`            - prints information about the given term
    * `ls/0`           - lists the contents of the current directory
    * `ls/1`           - lists the contents of the specified directory
    * `open/1`         - opens the source for the given module or function in your editor
    * `pid/1`          - creates a PID from a string
    * `pid/3`          - creates a PID with the 3 integer arguments passed
    * `port/1`         - creates a port from a string
    * `port/2`         - creates a port with the 2 non-negative integers passed
    * `ref/1`          - creates a reference from a string
    * `ref/4`          - creates a reference with the 4 integer arguments passed
    * `pwd/0`          - prints the current working directory
    * `r/1`            - recompiles the given module's source file
    * `recompile/0`    - recompiles the current project
    * `runtime_info/0` - prints runtime info (versions, memory usage, stats)
    * `v/0`            - retrieves the last value from the history
    * `v/1`            - retrieves the nth value from the history

  Help for all of those functions can be consulted directly from
  the command line using the `h/1` helper itself. Try:

      iex> h(v/0)

  To list all IEx helpers available, which is effectively all
  exports (functions and macros) in the `IEx.Helpers` module:

      iex> exports(IEx.Helpers)
  """
end
