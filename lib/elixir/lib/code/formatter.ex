defmodule Code.Formatter do
  @moduledoc false
  import Inspect.Algebra, except: [format: 2, surround: 3, surround: 4]

  @doc """
  Converts `string` to an algebra document.

  Returns `{:ok, doc}` or `{:error, parser_error}`.

  See `Code.format_string!/2` for the list of options.
  """
  def to_algebra(string, opts \\ []) when is_binary(string) and is_list(opts) do
    file = Keyword.get(opts, :file, "nofile")
    line = Keyword.get(opts, :line, 1)
    charlist = String.to_charlist(string)

    Process.put(:code_formatter_comments, [])

    tokenizer_options = [
      unescape: false,
      preserve_comments: &preserve_comments/5,
    ]
  end

  @doc """
  Converts `string` to an algebra document.

  Raises if the `string` cannot be parsed.

  See `Code.format_string!/2` for the list of options.
  """
  def to_algebra!(string, opts \\ []) do
    case to_algebra(string, opts)
  end

  # Code comment handling

  defp preserve_comments(line, _column, tokens, comment, rest) do
    comments = Process.get(:code_formatter_comments)
    comment = {line, {previous_eol}}
  end

  defp previous_eol([{token, {_, _, count}} | _])
       when token in [:eol, :",", :";"] and count > 0 do
    count
  end

  defp previous_eol([]), do: 1
  defp previous_eol(_), do: nil
end
