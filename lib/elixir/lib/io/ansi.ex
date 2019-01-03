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

  @doc ~S"""
  Formats a chardata-like argument by converting named ANSI sequences into actual
  ANSI codes.

  The named sequences are represented by atoms.

  An optional boolean parameter can be passed to enable or disable
  emitting actual ANSI codes. When `false`, no ANSI codes will be emitted.
  By default checks if ANSI is enabled using the `enabled?/0` function.

  ## Examples

      iex> IO.ANSI.format_fragment([:bright, 'Word'], true)
      [[[[[[] | "\e[1m]"], 87], 111], 114], 100]

  """
  def format_fragment(chardata, emit? \\ enabled?()) when is_boolean(emit?) do
    do_format(chardata, [], [], emit?, false)
  end

  defp do_format([term | rest], rem, acc, emit?, append_reset) do
    do_format(term, [rest | rem], acc, emit?, append_reset)
  end

  defp do_format(term, rem, acc, true, append_reset) when is_atom(term) do
    do_format([], rem, [acc | format_sequence(term)], true, !!append_reset)
  end

  defp do_format(term, rem, acc, false, append_reset) when not is_list(term) do
    do_format([], rem, acc, false, append_reset)
  end

  defp do_format(term, rem, acc, emit?, append_reset) when not is_list(term) do
    do_format([], rem, [acc, term], emit?, append_reset)
  end

  defp do_format([], [next | rest], acc, emit?, append_reset) do
    do_format(next, rest, acc, emit?, append_reset)
  end

  defp do_format([], [], acc, true, true) do
    [acc | IO.ANSI.reset()]
  end

  defp do_format([], [], acc, _emit?, _append_reset) do
    acc
  end
end
