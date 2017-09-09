defmodule Mix.Shell do
  @moduledoc """
  Defines `Mix.Shell` contract.
  """

  @doc """
  Prints the given message to the shell.
  """
  @callback info(message :: IO.ANSI.ansidata) :: any

  @doc """
  Prints the given error to the shell.
  """
  @callback error(message :: IO.ANSI.ansidata) :: any

  @doc """
  Prompts the user for input.
  """
  @callback prompt(message :: String.t) :: String.t

  @doc """
  Prompts the user for confirmation.
  """
  @callback yes?(message :: String.t) :: boolean

  @doc """
  Executes the given command and returns its exit status.
  """
  @callback cmd(command :: String.t) :: integer

  @doc """
  Executes the given command and returns its exit status.

  ## Options

    * `:print_app` - when `false`, does not print the app name
      when the command outputs something

    * `:stderr_to_stdout` - when `false`, does not redirect
      stderr to stdout

    * `:quiet` - when `true`, do not print the command output

    * `:env` - environment options to the executed command

  """
  @callback cmd(command :: String.t, options :: keyword) :: integer

  @doc """
  Prints the current application to the shell if
  it was not printed yet.
  """
  @callback print_app() :: any

  @doc """
  Returns the printable app name.

  This function returns the current application name,
  but only if the application name should be printed.

  Calling this function automatically toggles its value
  to `false` until the current project is re-entered. The
  goal is to avoid printing the application name
  multiple times.
  """
  def printable_app_name do
    Mix.ProjectStack.printable_app_name
  end
