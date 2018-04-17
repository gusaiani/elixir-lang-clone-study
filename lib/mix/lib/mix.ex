defmodule Mix do
  @moduledoc ~S"""
  Mix is a build tool that provides tasks for creating, compiling,
  and testing Elixir projects, managing its dependencies, and more.

  ## Mix.Project

  The foundation of Mix is a project. A project can be defined by using
  `Mix.Project` in a module, usually placed in a file named `mix.exs`:

      defmodule MyApp.MixProject do
        use Mix.Project

        def project do
          [
            app: :my_app,
            version: "1.0.0"
          ]
        end
      end

  See the `Mix.Project` module for detailed documentation on Mix projects.

  Once the project is defined, a number of default Mix tasks can be run
  directly from the command line:

    * `mix compile` - compiles the current project
    * `mix test` - runs tests for the given project
    * `mix run` - runs a particular command inside the project

  Each task has its own options and sometimes specific configuration
  to be defined in the `project/0` function. You can use `mix help`
  to list all available tasks and `mix help NAME` to show help for
  a particular task.

  The best way to get started with your first project is by calling
  `mix new my_project` from the command line.

  ## Mix.Task

  Tasks are what make Mix extensible.

  Projects can extend Mix behaviour by adding their own tasks. For
  example, adding the task below inside your project will
  make it available to everyone that uses your project:

      defmodule Mix.Tasks.Hello do
        use Mix.Task

        def run(_) do
          Mix.shell.info "hello"
        end
      end

  The task can now be invoked with `mix hello`.

  ## Dependencies

  Mix also manages your dependencies and integrates nicely with the [Hex package
  manager](https://hex.pm).

  In order to use dependencies, you need to add a `:deps` key
  to your project configuration. We often extract the list of dependencies
  into its own function:

      defmodule MyApp.MixProject do
        use Mix.Project

        def project do
          [
            app: :my_app,
            version: "1.0.0",
            deps: deps()
          ]
        end

        defp deps do
          [
            {:ecto, "~> 2.0"},
            {:plug, github: "elixir-lang/plug"}
          ]
        end
      end

  You can run `mix help deps` to learn more about dependencies in Mix.

  ## Environments

  Mix supports different environments. Environments allow developers to prepare
  and organize their project specifically for different scenarios. By default,
  Mix provides three environments:
  """
