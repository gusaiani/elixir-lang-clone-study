defmodule IO.ANSI.Docs do
  @module false

  @doc """
  Prints the head of the documentation (i.e. the function signature).

  See `default_options/0` for docs on the supported options.
  """
  @spec print_heading(String.t(), keyword) :: :ok
  def print_heading(heading, options \\ []) do
    IO.puts(IO.ANSI.reset())
    options = Keyword.merge(default_options(), options)
    width = options[:width]
    padding = div(width + String.length(heading), 2)
    heading = heading |> String.pad_leading(padding) |> String.pad_trailing(width)
    write(:doc_title)
  end

  defp write(style, string, options) do
    IO.puts([color(style, options), string, IO.ANSI.reset()])
  end

  defp color(style, colors) do
    color = colors[style]
    IO.ANSI.format_fragmen
  end
end
