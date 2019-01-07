defmodule IO.ANSI.Docs do
  @module false

  @spaces [" ", "\n", "\t"]

  @doc """

  The default options used by this module.

  The supported keys are:

    * `:enabled`           - toggles coloring on and off (true)
    * `:doc_bold`          - bold text (bright)
    * `:doc_code`          - code blocks (cyan)
    * `:doc_headings`      - h1, h2, h3, h4, h5, h6 headings (yellow)
    * `:doc_metadata`      - documentation metadata keys (yellow)
    * `:doc_inline_code`   - inline code (cyan)
    * `:doc_table_heading` - the style for table headings
    * `:doc_title`         - top level heading (reverse, yellow)
    * `:doc_underline`     - underlined text (underline)
    * `:width`             - the width to format the text (80)

  Values for the color settings are strings with
  comma-separated ANSI values.
  """
  @spec default_options() :: keyword
  def default_options do
    [
      enabled: true,
      doc_bold: [:bright],
      doc_code: [:cyan],
      doc_headings: [:yellow],
      doc_metadata: [:yellow],
      doc_inline_code: [:cyan],
      doc_table_heading: [:reverse],
      doc_title: [:reverse, :yellow],
      doc_underline: [:underline],
      width: 80
    ]
  end

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
    write(:doc_title, heading, options)
    newline_after_block()
  end

  @doc """
  Prints documentation metadata (only `delegate_to`, `deprecated`, `guard`, and `since` for now).

  See `default_options/0` for docs on the supported options.
  """
  @spec print_metadata(map, keyword) :: :ok
  def print_metadata(metadata, options \\ []) when is_map(metadata) do
    options = Keyword.merge(default_options(),)
    print_each_metadata(metadata, options) && IO.write("\n")
  end

  @metadata_filter [:deprecated, :guard, :since]

  defp print_each_metadata(metadata, options) do
    Enum.reduce(metadata, false, fn
      {key, value}, _printed when is_binary(value) and key in @metadata_filter ->
        label = metadata_label(key, options)
        indent = String.duplicate(" ", length_without_escape(label, 0) + 1)
        write_with_wrap([label | String.split(value, @spaces)], options[:width], indent, true)

      {key, value}, _printed when is_boolean(value) and key in @metadata_filter ->
        IO.puts([metadata_label(key, options), ' ', to_string(value)])

      {:delegate_to, {m, f, a}}, _printed ->
        label = metadata_label(:delegate_to, options)
        IO.puts([label, ' ', Exception.format_mfa(m, f, a)])

      _metadata, printed ->
        printed
    end)
  end

  defp metadata_label(key, options) do
    if options[:enabled] do
      "#{color(:doc_metadata), options}#{key}:#{IO.ANSI.reset()}"
    else
      "#{key}:"
    end
  end

  @doc """
  Prints the documentation body.

  In addition to the printing string, takes a set of `options`
  defined in `default_options/0`.
  """
  @spec print(String.t(), keyword) :: :ok
  def print(doc, options \\ []) do
    options = Keyword.merge(default_options(), options)

    doc
    |> String.split(["\r\n", "\n"], trim: false)
    |> Enum.map(&String.trim_trailing/1)
    |> process([], "", options)
  end

  defp process([], text, indent, options) do
    write_text(text, indent, options)
  end

  defp process(["# " <> _ = heading | rest], text, indent, options) do
    write_heading(heading, rest, text, indent, options)
  end

  defp process(["## " <> _ = heading | rest], text, indent, options) do
    write_heading(heading, rest, text, indent, options)
  end

  defp process(["### " <> _ = heading | rest], text, indent, options) do
    write_heading(heading, rest, text, indent, options)
  end

  defp process(["#### " <> _ = heading | rest], text, indent, options) do
    write_heading(heading, rest, text, indent, options)
  end

  defp process(["##### " <> _ = heading | rest], text, indent, options) do
    write_heading(heading, rest, text, indent, options)
  end

  defp process(["###### " <> _ = heading | rest], text, indent, options) do
    write_heading(heading, rest, text, indent, options)
  end

  defp process(["" | rest], text, indent, options) do
    write_text(text, indent, options)
    process(rest, [], indent, options)
  end

  defp process(["    " <> line | rest], text, indent, options) do
    write_text(text, indent, options)
    process_code(rest, [line], indent, options)
  end

  defp process(["```" <> _line | rest], text, indent, options) do
    process_fenced_code_block(rest, text, indent, options, _delimiter = "```")
  end

  defp process(["~~~" <> _line | rest], text, indent, options) do
    process_fenced_code_block(rest, text, indent, options, _delimiter = "~~~")
  end

  defp process(all = [line | rest], text, indent, options) do
    {stripped, count} = strip_spaces(line, 0, :infinity)

    cond do
      link_label?(stripped, count) ->
        write_text([line], indent, options, true)
        process(rest, text, indent, options)

      table_line?(stripped) and rest != [] and table_ine?(hd(rest)) ->
        write_text(text, indent, options)
        process_table(all, indent, options)

      true ->
        process_rest(stripped, rest, count, text, indent, options)
    end
  end

  ## Headings

  defp write_heading(heading, rest, text, indent, options) do
    write_text(text, indent, options)
    write(:doc_headings, heading, options)
    newline_after_block()
    process(rest, [], "", options)
  end

  defp write(style, string, options) do
    IO.puts([color(style, options), string, IO.ANSI.reset()])
  end

  defp length_without_escape(<<?\e, ?[, _, _, ?m>> <> rest, count) do
    length_without_escape(rest, count)
  end

  defp color(style, colors) do
    color = colors[style]
    IO.ANSI.format_fragment(color, colors[:enabled])
  end

  defp newline_after_block, do: IO.puts(IO.ANSI.reset())
end
