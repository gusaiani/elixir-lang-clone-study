defmodule Mix.Utils do
  @moduledoc false

  @doc """
  Gets the Mix home.

  It defaults to `~/.mix` unless the `MIX_HOME`
  environment variable is set.

  Developers should only store entries in the
  `MIX_HOME` directory which are guaranteed to
  work across multiple Elixir versions, as it is
  not recommended to swap the `MIX_HOME` directory
  as configuration and other important data may be
  stored there.
  """
  def mix_home do
    System.get_env("MIX_HOME") || Path.expand("~/.mix")
  end

  @doc """
  Gets all paths defined in the MIX_PATH env variable.

  `MIX_PATH` may contain multiple paths. If on Windows, those
  paths should be separated by `;`, if on Unix systems, use `:`.
  """
  def mix_paths do
    if path = System.get_env("MIX_PATH") do
      String.split(path, path_separator())
    else
      []
    end
  end

  defp path_separator do
    case :os.type do
      {:win32, _} -> ";"
      {:unix, _}  -> ":"
    end
  end

  @doc """
  Parses a string into module, function and arity.

  It returns `{:ok, mfa_list}`, where a `mfa_list` is
  `[module, function, arity]`, `[module, function]` or `[module]`,
  or the atom `:error`.

      iex> Mix.Utils.parse_mfa("Foo.bar/1")
      {:ok, [Foo, :bar, 1]}
      iex> Mix.Utils.parse_mfa(":foo.bar/1")
      {:ok, [:foo, :bar, 1]}
      iex> Mix.Utils.parse_mfa(":foo.bar")
      {:ok, [:foo, :bar]}
      iex> Mix.Utils.parse_mfa(":foo")
      {:ok, [:foo]}
      iex> Mix.Utils.parse_mfa("Foo")
      {:ok, [Foo]}

      iex> Mix.Utils.parse_mfa("Foo.")
      :error
      iex> Mix.Utils.parse_mfa("Foo.bar.baz")
      :error
      iex> Mix.Utils.parse_mfa("Foo.bar/2/2")
      :error
  """
  def parse_mfa(mfa) do
    with {:ok, quoted} <- Code.string_to_quoted(mfa),
         [_ | _] = mfa_list <- quoted_to_mfa(quoted) do
      {:ok, mfa_list}
    else
      _ -> :error
    end
  end

  defp quoted_to_mfa({:/, _, [dispatch, arity]}) when is_integer(arity) do
    quoted_to_mf(dispatch, [arity])
  end
  defp quoted_to_mfa(dispatch) do
    quoted_to_mf(dispatch, [])
  end

  defp quoted_to_mf({{:., _, [module, fun]}, _, []}, acc) when is_atom(fun) do
    quoted_to_m(module, [fun | acc])
  end
  defp quoted_to_mf(module, acc) do
    quoted_to_m(module, acc)
  end

  defp quoted_to_m({:__aliases__, _, aliases}, acc) do
    [Module.concat(aliases) | acc]
  end
  defp quoted_to_m(atom, acc) when is_atom(atom) do
    [atom | acc]
  end
  defp quoted_to_m(_, _acc) do
    []
  end

  @doc """
  Takes a `command` name and attempts to load a module
  with the command name converted to a module name
  in the given `at` scope.

  Returns `{:module, module}` in case a module
  exists and is loaded, `{:error, reason}` otherwise.

  ## Examples

      iex> Mix.Utils.command_to_module("compile", Mix.Tasks)
      {:module, Mix.Tasks.Compile}

  """
  def command_to_module(command, at \\ Elixir) do
    module = Module.concat(at, command_to_module_name(command))
    Code.ensure_loaded(module)
  end

  @doc """
  Returns `true` if any of the `sources` are stale
  compared to the given `targets`.
  """
  def stale?(sources, targets) do
    Enum.any? stale_stream(sources, targets)
  end

  @doc """
  Extracts all stale `sources` compared to the given `targets`.
  """
  def extract_stale(_sources, []), do: []
  def extract_stale([], _targets), do: []

  def extract_stale(sources, targets) do
    stale_stream(sources, targets) |> Enum.to_list
  end

  defp stale_stream(sources, targets) do
    modified_target = targets |> Enum.map(&last_modified/1) |> Enum.min

    Stream.filter(sources, fn(source) ->
      last_modified(source) > modified_target
    end)
  end

  @doc """
  Returns the date the given path was last modified.

  If the path does not exist, it returns the Unix epoch
  (1970-01-01 00:00:00).
  """
  def last_modified(path)

  def last_modified({{_, _, _}, {_, _, _}} = timestamp) do
    timestamp
  end

  def last_modified(path) do
    {mtime, _size} = last_modified_and_size(path)
    mtime
  end

  @doc false
  def last_modified_and_size(path) do
    now = :calendar.universal_time

    case :elixir_utils.read_mtime_and_size(path) do
      {:ok, mtime, size} when mtime > now ->
        Mix.shell.error("warning: mtime (modified time) for \"#{path}\" was set to the future, resetting to now")
