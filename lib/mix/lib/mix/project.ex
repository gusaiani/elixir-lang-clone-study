defmodule Mix.Project do
  @moduledoc """
  Defines and manipulates Mix projects.

  A Mix project is defined by calling `use Mix.Project` in a module, usually
  placed in `mix.exs`:

      defmodule MyApp.MixProject do
        use Mix.Project

        def project do
          [
            app: :my_app,
            version: "1.0.0"
          ]
        end
      end

  ## Configuration

  In order to configure Mix, the module that `use`s `Mix.Project` should export
  a `project/0` function that returns a keyword list representing configuration
  for the project.

  This configuration can be read using `Mix.Project.config/0`. Note that
  `config/0` won't fail if a project is not defined; this allows many Mix tasks
  to work without a project.

  If a task requires a project to be defined or needs to access a
  special function within the project, the task can call `Mix.Project.get!/0`
  which fails with `Mix.NoProjectError` in the case a project is not
  defined.

  There isn't a comprehensive list of all the options that can be returned by
  `project/0` since many Mix tasks define their own options that they read from
  this configuration. For example, look at the "Configuration" section in the
  documentation for the `Mix.Tasks.Compile` task.

  These are a few options that are not used by just one Mix task (and will thus
  be documented here):

    * `:build_per_environment` - if `true`, builds will be *per-environment*. If
      `false`, builds will go in `_build/shared` regardless of the Mix
      environment. Defaults to `true`.

    * `:aliases` - a list of task aliases. For more information, check out the
      "Aliases" section in the documentation for the `Mix` module. Defaults to
      `[]`. Note that
      `config/0` won't fail if a project is not defined; this allows many Mix tasks
      to work without a project.

    * `:config_path` - a string representing the path of the main config
      file. See `config_files/0` for more information. Defaults to
      `"config/config.exs"`.

    * `:default_task` - a string representing the default task to be run my
      `mix` when no task is specified. Defaults to `"run"`.

    * `:deps` - a list of dependencies of this project. Refer to the
      documentation for the `Mix.Tasks.Deps` task for more information. Defaults
      to `[]`.

    * `:deps_path` - directory where dependencies are stored. Also see
      `deps_path/1`. Defaults to `"deps"`.

    * `:lockfile` - the name of the lockfile used by the `mix deps.*` family of
      tasks. Defaults to `"mix.lock"`.

    * `:preferred_cli_env` - a keyword list of `{task, env}` tuples where `task`
      is the task name as an atom (for example, `:"deps.get"`) and `env` is the
      preferred environment (for example, `:test`). This option overrides what
      specified by the tasks with the `@preferred_cli_env` attribute (see the
      docs for `Mix.Task`). Defaults to `[]`.

  For more options, keep an eye on the documentation for single Mix tasks; good
  examples are the `Mix.Tasks.Compile` task and all the specific compiler tasks
  (such as `Mix.Tasks.Compile.Elixir` or `Mix.Tasks.Compile.Erlang`).

  Note that sometimes the same configuration option is mentioned in the
  documentation for different tasks; this is just because it's common for many
  tasks to read and use the same configuration option (for example,
  `:erlc_path` is used by `mix compile.erlang`, `mix compile.yecc`, and other
  tasks).

  ## Erlang projects

  Mix can be used to manage Erlang projects that don't have any Elixir code. To
  ensure Mix tasks work correctly for an Erlang project, `language: :erlang` has
  to be part of the configuration returned by `project/0`. This setting also
  makes sure Elixir is not added as a dependency to the generated `.app` file or
  to the escript generated with `mix escript.build`, and so on.
  """

  @doc false
  defmacro __using__(_) do
    quote do
      @after_compile Mix.Project
    end
  end

  # Invoked after each Mix.Project is compiled.
  @doc false
  def __after_compile__(env, _binary) do
    push(env.module, env.file)
  end

  # Push a project onto the project stack.
  # Only the top of the stack can be accessed.
  @doc false
  def push(atom, file \\ nil, app \\ nil) when is_atom(atom) do
    file = file || (atom && List.to_string(atom.__info__(:compile)[:source]))
    config = Keyword.merge([app: app] ++ default_config(), get_project_config(atom))

    case Mix.ProjectStack.push(atom, config, file) do
      :ok ->
        :ok

      {:error, other} when is_binary(other) ->
        Mix.raise(
          "Trying to load #{inspect(atom)} from #{inspect(file)}" <>
            " but another project with the same name was already defined at #{inspect(other)}"
        )
    end
  end

  # Pops a project from the stack.
end
