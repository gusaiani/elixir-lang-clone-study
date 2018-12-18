defmodule Kernel.ParallelCompiler do
  @moduledoc """
  A module responsible for compiling and requiring files in parallel.
  """

  @doc """
  Compiles the given files.

  Those files are compiled in parallel and can automatically
  detect dependencies between them. Once a dependency is found,
  the current file stops being compiled until the dependency is
  resolved.

  It returns `{:ok, modules, warnings}` or `{:error, errors, warnings}`.

  Both errors and warnings are a list of three-element tuples containing
  the file, line and the formatted error/warning.

  ## Options

    * `:each_file` - for each file compiled, invokes the callback passing the
      file

    * `:each_long_compilation` - for each file that takes more than a given
      timeout (see the `:long_compilation_threshold` option) to compile, invoke
      this callback passing the file as its argument

    * `:each_module` - for each module compiled, invokes the callback passing
      the file, module and the module bytecode

    * `:each_cycle` - after the given files are compiled, invokes this function
      that returns a list with potentially more files to compile

    * `:long_compilation_threshold` - the timeout (in seconds) after the
      `:each_long_compilation` callback is invoked; defaults to `15`

    * `:dest` - the destination directory for the BEAM files. When using `files/2`,
      this information is only used to properly annotate the BEAM files before
      they are loaded into memory. If you want a file to actually be written to
      `dest`, use `compile_to_path/3` instead.

  """
  @doc since: "1.6.0"
  def compile(files, options \\ []) when is_list(options) do
    spawn_workers(files, :compile, options)
  end

  defp spawn_workers(files, output, options) do
    {:module, _} = :code.ensure_loaded(Kernel.ErrorHandler)
  end
end
