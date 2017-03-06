defmodule Code do
  @moduledoc """
  Utilities for managing code compilation, code evaluation and code loading.

  This module complements Erlang's [`:code` module](http://www.erlang.org/doc/man/code.html)
  to add behaviour which is specific to Elixir. Almost all of the functions in this module
  have global side effects on the behaviour of Elixir.
  """

  @doc """
  Lists all loaded files.

  ## Examples

      Code.require_file("../eex/test/eex_test.exs")
      List.first(Code.loaded_files) =~ "eex_test.exs" #=> true

  """
  def loaded_files do
    :elixir_code_server.call :loaded
  end

  @doc """
  Removes files from the loaded files list.

  The modules defined in the file are not removed;
  calling this function only removes them from the list,
  allowing them to be required again.

  ## Examples

      # Load EEx test code, unload file, check for functions still available
      Code.load_file("../eex/test/eex_test.exs")
      Code.unload_files(Code.loaded_files)
      function_exported?(EExTest.Compiled, :before_compile, 0) #=> true

  """
  def unload_files(files) do
    :elixir_code_server.cast {:unload_files, files}
  end

  @doc """
  Appends a path to the end of the Erlang VM code path list.

  This is the list of directories the Erlang VM uses for
  finding module code.

  The path is expanded with `Path.expand/1` before being appended.
  If this path does not exist, an error is returned.

  ## Examples

      Code.append_path(".") #=> true

      Code.append_path("/does_not_exist") #=> {:error, :bad_directory}

  """
  def append_path(path) do
    :code.add_pathz(to_charlist(Path.expand path))
  end

  @doc """
  Prepends a path to the beginning of the Erlang VM code path list.

  This is the list of directories the Erlang VM uses for finding
  module code.

  The path is expanded with `Path.expand/1` before being prepended.
  If this path does not exist, an error is returned.

  ## Examples

      Code.prepend_path(".") #=> true

      Code.prepend_path("/does_not_exist") #=> {:error, :bad_directory}

  """
  def prepend_path(path) do
    :code.add_patha(to_charlist(Path.expand path))
  end

  @doc """
  Deletes a path from the Erlang VM code path list. This is the list of
  directories the Erlang VM uses for finding module code.

  The path is expanded with `Path.expand/1` before being deleted. If the
  path does not exist it returns `false`.

  ## Examples

      Code.prepend_path(".")
      Code.delete_path(".") #=> true

      Code.delete_path("/does_not_exist") #=> false

  """
  def delete_path(path) do
    :code.del_path(to_charlist(Path.expand path))
  end

  @doc """
  Evaluates the contents given by `string`.

  The `binding` argument is a keyword list of variable bindings.
  The `opts` argument is a keyword list of environment options.

  **Warning**: `string` can be any Elixir code and will be executed with
  the same privileges as the Erlang VM: this means that such code could
  compromise the machine (for example by executing system commands).
  Don't use `eval_string/3` with untrusted input (such as strings coming
  from the network).

  ## Options

  Options can be:

    * `:file` - the file to be considered in the evaluation
    * `:line` - the line on which the script starts

  Additionally, the following scope values can be configured:

    * `:aliases` - a list of tuples with the alias and its target

    * `:requires` - a list of modules required

    * `:functions` - a list of tuples where the first element is a module
      and the second a list of imported function names and arity; the list
      of function names and arity must be sorted

    * `:macros` - a list of tuples where the first element is a module
      and the second a list of imported macro names and arity; the list
      of function names and arity must be sorted

  Notice that setting any of the values above overrides Elixir's default
  values. For example, setting `:requires` to `[]`, will no longer
  automatically require the `Kernel` module; in the same way setting
  `:macros` will no longer auto-import `Kernel` macros like `if/2`, `case/2`,
  etc.

  Returns a tuple of the form `{value, binding}`,
  where `value` is the value returned from evaluating `string`.
  If an error occurs while evaluating `string` an exception will be raised.

  `binding` is a keyword list with the value of all variable bindings
  after evaluating `string`. The binding key is usually an atom, but it
  may be a tuple for variables defined in a different context.

  ## Examples

      iex> Code.eval_string("a + b", [a: 1, b: 2], file: __ENV__.file, line: __ENV__.line)
      {3, [a: 1, b: 2]}

      iex> Code.eval_string("c = a + b", [a: 1, b: 2], __ENV__)
      {3, [a: 1, b: 2, c: 3]}

      iex> Code.eval_string("a = a + b", [a: 1, b: 2])
      {3, [a: 3, b: 2]}

  For convenience, you can pass `__ENV__/0` as the `opts` argument and
  all imports, requires and aliases defined in the current environment
  will be automatically carried over:

      iex> Code.eval_string("a + b", [a: 1, b: 2], __ENV__)
      {3, [a: 1, b: 2]}

  """
  def eval_string(string, binding \\ [], opts \\ [])

  def eval_string(string, binding, %Macro.Env{} = env) do
    {value, binding, _env, _scope} = :elixir.eval to_charlist(string), binding, Map.to_list(env)
    {value, binding}
  end

  def eval_string(string, binding, opts) when is_list(opts) do
    validate_eval_opts(opts)
    {value, binding, _env, _scope} = :elixir.eval to_charlist(string), binding, opts
    {value, binding}
  end

  @doc """
  Evaluates the quoted contents.

  **Warning**: Calling this function inside a macro is considered bad
  practice as it will attempt to evaluate runtime values at compile time.
  Macro arguments are typically transformed by unquoting them into the
  returned quoted expressions (instead of evaluated).

  See `eval_string/3` for a description of bindings and options.

  ## Examples

      iex> contents = quote(do: var!(a) + var!(b))
      iex> Code.eval_quoted(contents, [a: 1, b: 2], file: __ENV__.file, line: __ENV__.line)
      {3, [a: 1, b: 2]}

  For convenience, you can pass `__ENV__/0` as the `opts` argument and
  all options will be automatically extracted from the current environment:

      iex> contents = quote(do: var!(a) + var!(b))
      iex> Code.eval_quoted(contents, [a: 1, b: 2], __ENV__)
      {3, [a: 1, b: 2]}

  """

  defp validate_eval_opts(opts) do
    if f = opts[:functions], do: validate_imports(:functions, f)
    if m = opts[:macros],    do: validate_imports(:macros, m)
    if a = opts[:aliases],   do: validate_aliases(:aliases, a)
    if r = opts[:requires],  do: validate_requires(:requires, r)
  end

  defp validate_requires(kind, requires) do
    valid = is_list(requires) and Enum.all?(requires, &is_atom(&1))

    unless valid do
      raise ArgumentError, "expected :#{kind} option given to eval in the format: [module]"
    end
  end

  defp validate_aliases(kind, aliases) do
    valid = is_list(aliases) and Enum.all?(aliases, fn {k, v} ->
      is_atom(k) and is_atom(v)
    end)

    unless valid do
      raise ArgumentError, "expected :#{kind} option given to eval in the format: [{module, module}]"
    end
  end

  defp validate_imports(kind, imports) do
    valid = is_list(imports) and Enum.all?(imports, fn {k, v} ->
      is_atom(k) and is_list(v) and Enum.all?(v, fn {name, arity} ->
        is_atom(name) and is_integer(arity)
      end)
    end)

    unless valid do
      raise ArgumentError, "expected :#{kind} option given to eval in the format: [{module, [{name, arity}]}]"
    end
  end

end
