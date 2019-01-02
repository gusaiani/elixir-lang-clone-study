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

  This module also includes helpers for debugging purposes, see
  `IEx.break!/4` for more information.

  To learn more about IEx as a whole, type `h(IEx)`.
  """

  import IEx, only: [dont_display_result: 0]

  @doc """
  Recompiles the current Mix application.

  This helper only works when IEx is started with a Mix
  project, for example, `iex -S mix`. The application is
  not restarted after compilation, which means any long
  running process may crash as any changed module will be
  temporarily removed and recompiled, without going through
  the proper code changes callback.

  If you want to reload a single module, consider using
  `r(ModuleName)` instead.

  This function is meant to be used for development and
  debugging purposes. Do not depend on it in production code.

  ## Options

    * `:force` - when `true`, forces the application to recompile

  """
  def recompile(options \\ []) do
    if mix_started?() do
      config = Mix.Project.config()
      consolidation = Mix.Project.consolidation_path(config)
      reenable_tasks(config)

      # No longer allow consolidations to be accessed.
      Code.delete_path(consolidation)
      purge_protocols(consolidation)

      force? = Keyword.get(options, :force, false)
      arguments = if force?, do: ["--force"], else: []

      {result, _} = Mix.Task.run("compile", arguments)

      # Reenable consolidation and allow them to be loaded.
      Code.prepend_path(consolidation)
      purge_protocols(consolidation)

      result
    else
      IO.puts(IEx.color(:eval_error, "Mix is not running. Please start IEx with: iex -S mix"))
      :error
    end
  end

  defp mix_started? do
    List.keyfind(Application.started_applications(), :mix, 0) != nil
  end

  defp reenable_tasks(config) do
    Mix.Task.reenable("compile")
    Mix.Task.reenable("compile.all")
    Mix.Task.reenable("compile.protocols")
    compilers = config[:compilers] || Mix.compilers()
    Enum.each(compilers, &Mix.Task.reenable("compile.#{&1}"))
  end

  defp purge_protocols(path) do
    case File.ls(path) do
      {:ok, beams} ->
        Enum.each(beams, fn beam ->
          module = beam |> Path.rootname() |> String.to_atom()
          :code.purge(module)
          :code.delete(module)
        end)

      {:error, _} ->
        :ok
    end
  end

  @doc """
  Compiles the given files.

  It expects a list of files to compile and an optional path to write
  the compiled code to. By default files are in-memory compiled.
  To write compiled files to the current directory, an empty string
  can be given.

  It returns the names of the compiled modules.

  If you want to recompile an existing module, check `r/1` instead.

  ## Examples

  In the example below, we pass a directory to where the `c/2` function will
  write the compiled `.beam` files to. This directory is typically named "ebin"
  in Erlang/Elixir systems:

      iex> c(["foo.ex", "bar.ex"], "ebin")
      [Foo, Bar]

  When compiling one file, there is no need to wrap it in a list:

      iex> c("baz.ex")
      [Baz]

  """
  def c(files, path \\ :in_memory) when is_binary(path) or path == :in_memory do
    files = List.wrap(files)

    unless Enum.all?(files, &is_binary/1) do
      raise ArgumentError, "expected a binary or a list of binaries as argument"
    end

    {found, not_found} = Enum.split_with(files, &File.exists?/1)

    unless Enum.empty?(not_found) do
      raise ArgumentError, "could not find files #{Enum.join(not_found, ", ")}"
    end

    {erls, exs} = Enum.split_with(found, &String.ends_with?(&1, ".erl"))

    erl_modules =
      Enum.map(erls, fn source ->
        {module, binary} = compile_erlang(source)

        if path != :in_memory do
          base = source |> Path.basename() |> Path.rootname()
          File.write!(Path.join(path, base <> ".beam"), binary)
        end

        module
      end)

    ex_modules =
      case compile_elixir(exs, path) do
        {:ok, modules, _} -> modules
        {:error, _, _} -> raise CompileError
      end

    erl_modules ++ ex_modules
  end

  @doc """
  Clears the console screen.

  This function only works if ANSI escape codes are enabled
  on the shell, which means this function is by default
  unavailable on Windows machines.
  """
  def clear() do
    if IO.ANSI.enabled?() do
      IO.write([IO.ANSI.home(), IO.ANSI.clear()])
    else
      IO.puts("Cannot clear the screen because ANSI escape codes are not enabled on this shell")
    end

    dont_display_result()
  end

  @doc """
  Opens the current prying location.

  This command only works inside a pry session started manually
  via `IEx.pry/0` or a breakpoint set via `IEx.break!/4`. Calling
  this function during a regular `IEx` session will print an error.

  Keep in mind the `open/0` location may not exist when prying
  precompiled source code, such as Elixir itself.

  For more information and to open any module or function, see
  `open/1`.
  """
  def open() do
    case Process.get(:iex_whereami) do
      {file, line, _} ->
        IEx.Introspection.open({file, line})

      _ ->
        IO.puts(IEx.color(:eval_error, "Pry session is not currently enabled"))
    end

    dont_display_result()
  end

  @doc """
  Opens the given module, module/function/arity or `{file, line}`.

  This function uses the `ELIXIR_EDITOR` environment variable
  and falls back to `EDITOR` if the former is not available.

  By default, it attempts to open the file and line using the
  `file:line` notation. For example, if your editor is called
  `subl`, it will open the file as:

      subl path/to/file:line

  It is important that you choose an editor command that does
  not block nor that attempts to run an editor directly in the
  terminal. Command-line based editors likely need extra
  configuration so they open up the given file and line in a
  separate window.

  Custom editors are supported by using the `__FILE__` and
  `__LINE__` notations, for example:

      ELIXIR_EDITOR="my_editor +__LINE__ __FILE__"

  and Elixir will properly interpolate values.

  Since this function prints the result returned by the editor,
  `ELIXIR_EDITOR` can be set "echo" if you prefer to display the
  location rather than opening it.

  Keep in mind the location may not exist when opening precompiled
  source code.

  ## Examples

      iex> open(MyApp)
      iex> open(MyApp.fun/2)
      iex> open({"path/to/file", 1})

  """
  defmacro open(term) do
    quote do
      IEx.Introspection.open(unquote(IEx.Introspection.decompose(term, __CALLER__)))
    end
  end

  @doc """
  Prints the documentation for `IEx.Helpers`.
  """
  def h() do
    IEx.Introspection.h(IEx.Helpers)
  end

  defp compile_elixir(exs, :in_memory), do: Kernel.ParallelCompiler.compile(exs)

  # Compiles and loads an Erlang source file, returns {module, binary}
  defp compile_erlang(source) do
    source = Path.relative_to_cwd(source) |> String.to_charlist

    case :compile.file(source, [:binary, :report]) do
      {:ok, module, binary} ->
        :code.purge(module)
        {:module, module} = :code.load_binary(module, source, binary)
        {module, binary}

      _ ->
        raise CompileError
    end
  end
end
