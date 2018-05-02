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
