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

