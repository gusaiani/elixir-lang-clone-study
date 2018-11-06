defmodule IEx do
  @moduledoc ~S"""
  Elixir's interactive shell.

  Some of the functionalities described here will not be available
  depending on your terminal. In particular, if you get a message
  saying that the smart terminal could not be run, some of the
  features described here won't work.

  ## Helpers

  IEx provides a bunch of helpers. They can be accessed by typing
  `h()` into the shell or as a documentation for the `IEx.Helpers` module.

  ## Autocomplete

  To discover a module's public functions or other modules, type the module name
  followed by a dot, then press tab to trigger autocomplete. For example:

      Enum.

  A module may export functions that are not meant to be used directly: these
  functions won't be autocompleted by IEx. IEx will not autocomplete functions
  annotated with `@doc false`, `@impl true`, or functions that aren't explicitly
  documented and where the function name is in the form of `__foo__`.

  Autocomplete may not be available on some Windows shells. You may need
  to pass the `--werl` flag when starting IEx, as in `iex --werl` for it
  to work. `--werl` may be permanently enabled by setting the `IEX_WITH_WERL`
  environment variable.

  ## Shell history

  It is possible to get shell history by passing some flags that enable it
  in the VM. This can be done on a per-need basis when starting IEx:

      iex --erl "-kernel shell_history enabled"

  If you would rather enable it on your system as a whole, you can use
  the `ERL_AFLAGS` environment variable and make sure that it is set
  accordingly on your terminal/shell configuration.

  On Linux:

      export ERL_AFLAGS="-kernel shell_history enabled"

  On Windows:

      set ERL_AFLAGS "-kernel shell_history enabled"

  ## Expressions in IEx

  As an interactive shell, IEx evaluates expressions. This has some
  interesting consequences that are worth discussing.

  The first one is that the code is truly evaluated and not compiled.
  This means that any benchmarking done in the shell is going to have
  skewed results. So never run any profiling nor benchmarks in the shell.

  Second, IEx allows you to break an expression into many lines,
  since this is common in Elixir. For example:

      iex(1)> "ab
      ...(1)> c"
      "ab\nc"

  In the example above, the shell will be expecting more input until it
  finds the closing quote. Sometimes it is not obvious which character
  the shell is expecting, and the user may find themselves trapped in
  the state of incomplete expression with no ability to terminate it other
  than by exiting the shell.

  For such cases, there is a special break-trigger (`#iex:break`) that when
  encountered on a line by itself will force the shell to break out of any
  pending expression and return to its normal state:

      iex(1)> ["ab
      ...(1)> c"
      ...(1)> "
      ...(1)> ]
      ...(1)> #iex:break
      ** (TokenMissingError) iex:1: incomplete expression

  ## Pasting multiline expressions into IEx

  IEx evaluates its input line by line in an eagerly fashion which means
  that if at the end of a line the code seen so far is a complete expression
  IEx will evaluate it at that point. This behaviour may produce errors for
  expressions that have been formatted across multiple lines which is often
  the case for piped expressions. Consider the following expression using
  the `|>/2` operator:

      iex(1)> [1, [2], 3] |> List.flatten()
      [1, 2, 3]

  When written in multiline form and pasted into IEx this valid expression
  produces a syntax error:

      iex(1)> [1, [2], 3]
      [1, [2], 3]
      iex(2)> |> List.flatten()
      ** (SyntaxError) iex:2: syntax error before: '|>'

  As IEx evaluates its input line by line, it will first encounter
  `[1, [2], 3]`. As a list is a valid expression, IEx will evaluate
  it immediately before looking at the next input line. Only then
  will IEx attempt to evaluate the now incomplete expression
  `|> List.flatten()`, which on its own is missing its left operand.
  The evaluation thus fails with the above syntax error.

  In order to help IEx understand that an expression consists of multiple
  lines we can wrap it into parentheses:

      iex(1)> (
      ...(1)> [1, [2], 3]
      ...(1)> |> List.flatten()
      ...(1)> )
      [1, 2, 3]
  """
end
