defmodule IO.ANSI.Sequence do
  @moduledoc false

  defmacro defsequence(name, code, terminator \\ "m") do
    quote bind_quoted: [name: name, code: code, terminator: terminator] do
      def unquote(name)() do
        "\e[#{unquote(code)}#{unquote(terminator)}"
      end

      defp format_sequence(unquote(name)) do
        unquote(name)()
      end
    end
  end
end

defmodule IO.ANSI do
  @moduledoc """
  Functionality to render ANSI escape sequences.

  [ANSI escape sequences](https://en.wikipedia.org/wiki/ANSI_escape_code)
  are characters embedded in text used to control formatting, color, and
  other output options on video text terminals.
  """

  import IO.ANSI.Sequence

  @type ansicode :: atom
  @type ansilist ::
          maybe_improper_list(char | ansicode | binary | ansilist, binary | ansicode | [])
  @type ansidata :: ansilist | ansicode | binary

  @doc """
  Checks if ANSI coloring is supported and enabled on this machine.

  This function simply reads the configuration value for
  `:ansi_enabled` in the `:elixir` application. The value is by
  default `false` unless Elixir can detect during startup that
  both `stdout` and `stderr` are terminals.
  """
  @spec enabled? :: boolean
  def enabled? do
    Application.get_env(:elixir, :ansi_enabled, false)
  end

  @doc "Sends cursor home."
  defsequence(:home, "", "H")

  @doc "Clears screen."
  defsequence(:clear, "2", "J")
end
