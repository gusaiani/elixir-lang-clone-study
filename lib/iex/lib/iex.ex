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

  Note that this not only works with single expressions but also with
  arbitrary code blocks.

  ## The BREAK menu

  Inside IEx, hitting `Ctrl+C` will open up the `BREAK` menu. In this
  menu you can quit the shell, see process and ETS tables information
  and much more.

  ## Exiting the shell

  There are a few ways to quit the IEx shell:

    * via the `BREAK` menu (available via `Ctrl+C`) by typing `q`, pressing enter
    * by hitting `Ctrl+C`, `Ctrl+C`
    * by hitting `Ctrl+\`

  If you are connected to remote shell, it remains alive after disconnection.

  ## Prying and breakpoints

  IEx also has the ability to set breakpoints on Elixir code and
  "pry" into running processes. This allows the developer to have
  an IEx session run inside a given function.

  `IEx.pry/0` can be used when you are able to modify the source
  code directly and recompile it:

      def my_fun(arg1, arg2) do
        require IEx; IEx.pry()
        ... implementation ...
      end

  When the code is executed, it will ask you for permission to be
  introspected.

  Alternatively, you can use `IEx.break!/4` to setup a breakpoint
  on a given module, function and arity you have no control of.
  While `IEx.break!/4` is more flexible, it does not contain
  information about imports and aliases from the source code.

  ## The User switch command

  Besides the `BREAK` menu, one can type `Ctrl+G` to get to the
  `User switch command` menu. When reached, you can type `h` to
  get more information.

  In this menu, developers are able to start new shells and
  alternate between them. Let's give it a try:

      User switch command
       --> s 'Elixir.IEx'
       --> c

  The command above will start a new shell and connect to it.
  Create a new variable called `hello` and assign some value to it:

      hello = :world

  Now, let's roll back to the first shell:

      User switch command
       --> c 1

  Now, try to access the `hello` variable again:

      hello
      ** (UndefinedFunctionError) undefined function hello/0

  The command above fails because we have switched shells.
  Since shells are isolated from each other, you can't access the
  variables defined in one shell from the other one.

  The `User switch command` can also be used to terminate an existing
  session, for example when the evaluator gets stuck in an infinite
  loop or when you are stuck typing an expression:

      User switch command
       --> i
       --> c

  The `User switch command` menu also allows developers to connect to
  remote shells using the `r` command. A topic which we will discuss next.

  ## Remote shells

  IEx allows you to connect to another node in two fashions.
  First of all, we can only connect to a shell if we give names
  both to the current shell and the shell we want to connect to.

  Let's give it a try. First start a new shell:

      $ iex --sname foo
      iex(foo@HOST)1>

  The string between the parentheses in the prompt is the name
  of your node. We can retrieve it by calling the `node/0`
  function:

      iex(foo@HOST)1> node()
      :"foo@HOST"
      iex(foo@HOST)2> Node.alive?()
      true

  For fun, let's define a simple module in this shell too:

      iex(foo@HOST)3> defmodule Hello do
      ...(foo@HOST)3>   def world, do: "it works!"
      ...(foo@HOST)3> end

  Now, let's start another shell, giving it a name as well:

      $ iex --sname bar
      iex(bar@HOST)1>

  If we try to dispatch to `Hello.world`, it won't be available
  as it was defined only in the other shell:

      iex(bar@HOST)1> Hello.world
      ** (UndefinedFunctionError) undefined function Hello.world/0

  However, we can connect to the other shell remotely. Open up
  the `User switch command` prompt (Ctrl+G) and type:

      User switch command
       --> r 'foo@HOST' 'Elixir.IEx'
       --> c

  Now we are connected into the remote node, as the prompt shows us,
  and we can access the information and modules defined over there:

      iex(foo@HOST)1> Hello.world
      "it works"

  In fact, connecting to remote shells is so common that we provide
  a shortcut via the command line as well:

      $ iex --sname baz --remsh foo@HOST

  Where "remsh" means "remote shell". In general, Elixir supports:

    * remsh from an Elixir node to an Elixir node
    * remsh from a plain Erlang node to an Elixir node (through the ^G menu)
    * remsh from an Elixir node to a plain Erlang node (and get an `erl` shell there)

  Connecting an Elixir shell to a remote node without Elixir is
  **not** supported.

  """
end
