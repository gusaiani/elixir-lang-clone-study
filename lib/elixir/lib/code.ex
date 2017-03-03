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
    :elixir_code_server.cast {:unload_files, file}
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
  """
end
