defmodule Mix.Config do
  @moduledoc ~S"""
  Module for defining, reading and merging app configurations.

  Most commonly, this module is used to define your own configuration:

      use Mix.Config

      config :plug,
        key1: "value1",
        key2: "value2"

      import_config "#{Mix.env}.exs"

  All `config/*` macros, including `import_config/1`, are used
  to help define such configuration files.

  Furthermore, this module provides functions like `read!/1`,
  `merge/2` and friends which help manipulate configurations
  in general.

  Configuration set using `Mix.Config` will set the application env, so
  that `Application.get_env/3` and other `Application` functions can be used
  at run or compile time to retrieve or change the configuration.

  For example, the `:key1` value from application `:plug` (see above) can be
  retrieved with:

      "value1" = Application.fetch_env!(:plug, :key1)

  """

  defmodule LoadError do
    defexception [:file, :error]

    def message(%LoadError{file: file, error: error}) do
      "could not load config #{Path.relative_to_cwd(file)}\n" <>
        "#{Exception.format_banner(:error, error)}"
    end
  end

  @doc false
  defmacro __using__(_) do
    quote do
      import Mix.Config, only: [config: 2, config: 3, import_config: 1]
      {:ok, agent} = Mix.Config.Agent.start_link()
      var!(config_agent, Mix.Config) = agent
    end
  end

  @doc """
  Configure the given application.

  Keyword lists are always deep merged.

  ## Examples

  The given `opts` are merged into the existing configuration
  for the given `app`. Conflicting keys are overridden by the
  ones specified in `opts`. For example, the declaration below:

      config :lager,
        log_level: :warn,
        mode: :truncate

      config :lager,
        log_level: :info,
        threshold: 1024

  Will have a final configuration of:

      [log_level: :info, mode: :truncate, threshold: 1024]

  This final configuration can be retrieved at run or compile time:

      Application.get_all_env(:lager)

  """
  defmacro config(app, opts) do
    quote do
      Mix.Config.Agent.merge(var!(config_agent, Mix.Config), [{unquote(app), unquote(opts)}])
    end
  end

  @doc """
  Configures the given key for the given application.

  Keyword lists are always deep merged.

  ## Examples

  The given `opts` are merged into the existing values for `key`
  in the given `app`. Conflicting keys are overridden by the
  ones specified in `opts`. For example, given the two configurations
  below:

      config :ecto, Repo,
        log_level: :warn,
        adapter: Ecto.Adapters.Postgres

      config :ecto, Repo,
        log_level: :info,
        pool_size: 10

  the final value fo the configuration for the `Repo` key in the `:ecto`
  application will be:

      [log_level: :info, pool_size: 10, adapter: Ecto.Adapters.Postgres]

  This final value can be retrieved at runtime or compile time with:

      Application.get_env(:ecto, Repo)

  """
  defmacro config(app, key, opts) do
    quote do
      Mix.Config.Agent.merge(var!(config_agent, Mix.Config), [
        {unquote(app), [{unquote(key), unquote(opts)}]}
      ])
    end
  end

  @doc ~S"""
  Imports configuration from the given file or files.

  If `path_or_wildcard` is a wildcard, then all the files
  matching that wildcard will be imported; if no file matches
  the wildcard, no errors are raised. If `path_or_wildcard` is
  not a wildcard but a path to a single file, then that file is
  imported; in case the file doesn't exist, an error is raised.
  This behaviour is analogous to the one for `read_wildcard!/1`.

  If path/wildcard is a relative path/wildcard, it will be expaded relatively
  to the directory the current configuration file is in.

  ## Examples

  This is often used to emulate configuration across environments:

      import_config "#{Mix.env}.exs"

  Or to import files from children in umbrella projects:

      import_config "../apps/*/config/config.exs"

  """
  defmacro import_config(path_or_wildcard) do
    loaded_paths_quote =
      unless Macro.Env.has_var?(__CALLER__, {:loaded_paths, Mix.Config}) do
        quote do
          var!(loaded_paths, Mix.Config) = [__ENV__.file]
        end
      end

    quote do
      unquote(loaded_paths_quote)

      Mix.Config.Agent.merge(
        var!(config_agent, Mix.Config)
        Mix.Config.read_wildcard!(
          Path.expand(unquote(path_or_wildcard), __DIR__),
          var!(loaded_paths, Mix.Config)
        )
      )
    end
  end

  @doc """
  Reads and validates a configuration file.

  `file` is the path to the configuration file to be read. If that file doesn't
  exist or if there's an error loading it, a `Mix.Config.LoadError` exception
  will be raised.

  `loaded_paths` is a list of configuration files that have been previously
  read. If `file` exists in `loaded_paths`, a `Mix.Config.LoadError` exception
  will be raised.
  """
  def read!(file, loaded_paths \\ []) do
    try do
      if file in loaded_paths do
        raise ArgumentError, message: "recursive load of #{file} detected"
      end

      binding = [{{:loaded_paths, Mix.Config}, [file | loaded_paths]}]

      {config, binding} =
        Code.eval_string(
          File.read!(file),
          binding,
          file: file,
          line: 1
        )

      config =
        case List.keyfind(binding, {:config_agent, Mix.Config}, 0) do
          {_, agent} -> get_config_and_stop_agent(agent)
          nil -> config
        end

      validate!(config)
      config
    rescue
      e in [LoadError] -> reraise(e, __STACKTRACE__)
      e -> reraise(LoadError, [file: file, error: e], __STACKTRACE__)
    end
  end

  defp get_config_and_stop_agent(agent) do
    config = Mix.Config.Agent.get(agent)
    Mix.Config.Agent.stop(agent)
    config
  end

  @doc """
  Reads many configuration files given by wildcard into a single config.

  Raises an error if `path` is a concrete filename (with no wildcards)
  but the corresponding file does not exist; if `path` matches no files,
  no errors are raised.

  `loaded_paths` is a list of configuration files that have been previously
  read.
  """
  def read_wildcard!(path, loaded_paths \\ []) do
    paths =
      if String.contains?(path, ~w(* ? [ {])) do
        Path.wildcard(path)
      else
        [path]
      end

    Enum.reduce(paths, [], &merge(&2, read!(&1, loaded_paths)))
  end

  @doc """
  Validates a configuration.
  """
  def validate!(config) do
    if is_list(config) do
      Enum.all?(config, fn
        {app, value} when is_atom(app) ->
          if Keyword.keyword?(value) do
            true
          else
            raise ArgumentError,
                  "expected config for app #{inspect(app)} to return keyword list, " <>
                    "got: #{inspect(value)}"
          end

        _ ->
          false
      end)
    else
      raise ArgumentError, "expected config file to return keyword list, got: #{inspect(config)}"
    end
  end
