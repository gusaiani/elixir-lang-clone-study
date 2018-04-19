defmodule Mix.Task.Compiler do
  @moduledoc """
  This module defines the behaviour for a Mix task that does compilation.

  A Mix compiler task can be defined by simply using `Mix.Task.Compiler`
  in a module whose name starts with `Mix.Tasks.Compile.` and defining
  the `run/1` function:

      defmodule Mix.Tasks.Compile.MyLanguage do
        use Mix.Task.Compiler

        def run(_args) do
          :ok
        end
      end

  The `run/1` function returns an atom indicating the status of the
  compilation, and optionally can also return a list of "diagnostics"
  such as warnings or compilation errors. Doing this enables code
  editors to display issues inline without having to analyze the
  command-line output.

  If the compiler uses manifest files to track stale sources, is should
  define `manifests/0`, and if it writes any output to disk it should
  also define `clean/0`.

  A compiler supports the same attributes for configuration and
  documentation as a regular Mix task. See `Mix.Task` for more information.
  """

  defmodule Diagnostic do
    @moduledoc """
    Diagnostic information such as a warning or compilation error.
    """

    @type t :: %__MODULE__{
            file: Path.t(),
            severity: severity,
            message: String.t(),
            position: position,
            compiler_name: String.t(),
            details: any
          }

    @typedoc """
    Severity of a diagnostic:

      * `:error` - An issue that caused compilation to fail

      * `:warning` - An issue that did not cause failure but suggests the
        programmer may have made a mistake

      * `:hint` - A suggestion for style or good practices that is not as
        sevre as a warning

      * `:information` - Any other information relevant to compilation that
        does not fit into the above categories

    """
    @type severity :: :error | :warning | :information | :hint

    @typedoc """
    Where in a file the diagnostic applies. Can be einther a line number,
    a range specified as `{start_line, start_col, end_line, end_col}`,
    or `nil` if unknown.

    Line numbers are 1-based, and column numbers in a range are 0-based and refer
    to the cursor position at the start of the character at that index. For example,
    to indicate that a diagnostic applies to the first `n` characters of the
    first line, the range would be `{1, 0, 1, n}`.
    """