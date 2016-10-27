import Kernel, except: [length: 1]

defmodule String do
  @moduledoc ~S"""
  A String in Elixir is a UTF-8 encoded binary.

  ## Codepoints and grapheme cluster

  The functions in this module act according to the Unicode
  Standard, version 6.3.0.

  As per the standard, a codepoint is a single Unicode Character,
  which may be represented by one or more bytes.

  For example, the codepoint "é" is two bytes:

      iex> byte_size("é")
      2

  However, this module returns the proper length:

      iex> String.length("é")
      1

  Furthermore, this module also presents the concept of grapheme cluster
  (from now on referenced as graphemes). Graphemes can consist of multiple
  codepoints that may be perceived as a single character by readers. For
  example, "é" can be represented either as a single "e with acute" codepoint
  or as the letter "e" followed by a "combining acute accent" (two codepoints):

      iex> string = "\u0065\u0301"
      iex> byte_size(string)
      3
      iex> String.length(string)
      1
      iex> String.codepoints(string)
      ["e", "́"]
      iex> String.graphemes(string)
      ["é"]

  Although the example above is made of two characters, it is
  perceived by users as one.

  Graphemes can also be two characters that are interpreted
  as one by some languages. For example, some languages may
  consider "ch" as a single character. However, since this
  information depends on the locale, it is not taken into account
  by this module.

  In general, the functions in this module rely on the Unicode
  Standard, but do not contain any of the locale specific behaviour.

  More information about graphemes can be found in the [Unicode
  Standard Annex #29](http://www.unicode.org/reports/tr29/).
  The current Elixir version implements Extended Grapheme Cluster
  algorithm.

  ## String and binary operations

  To act according to the Unicode Standard, many functions
  in this module run in linear time, as they need to traverse
  the whole string considering the proper Unicode codepoints.

  For example, `String.length/1` will take longer as
  the input grows. On the other hand, `Kernel.byte_size/1` always runs
  in constant time (i.e. regardless of the input size).

  This means often there are performance costs in using the
  functions in this module, compared to the more low-level
  operations that work directly with binaries:

    * `Kernel.binary_part/3` - retrieves part of the binary
    * `Kernel.bit_size/1` and `Kernel.byte_size/1` - size related functions
    * `Kernel.is_bitstring/1` and `Kernel.is_binary/1` - type checking function
    * Plus a number of functions for working with binaries (bytes)
      in the [`:binary` module](http://www.erlang.org/doc/man/binary.html)

  There are many situations where using the `String` module can
  be avoided in favor of binary functions or pattern matching.
  For example, imagine you have a string `prefix` and you want to
  remove this prefix from another string named `full`.

  One may be tempted to write:

      iex> take_prefix = fn full, prefix ->
      ...>   base = String.length(prefix)
      ...>   String.slice(full, base, String.length(full) - base)
      ...> end
      iex> take_prefix.("Mr. John", "Mr. ")
      "John"

  Although the function above works, it performs poorly. To
  calculate the length of the string, we need to traverse it
  fully, so we traverse both `prefix` and `full` strings, then
  slice the `full` one, traversing it again.

  A first attempt at improving it could be with ranges:

      iex> take_prefix = fn full, prefix ->
      ...>   base = String.length(prefix)
      ...>   String.slice(full, base..-1)
      ...> end
      iex> take_prefix.("Mr. John", "Mr. ")
      "John"

  While this is much better (we don't traverse `full` twice),
  it could still be improved. In this case, since we want to
  extract a substring from a string, we can use `Kernel.byte_size/1`
  and `Kernel.binary_part/3` as there is no chance we will slice in
  the middle of a codepoint made of more than one byte:

      iex> take_prefix = fn full, prefix ->
      ...>   base = byte_size(prefix)
      ...>   binary_part(full, base, byte_size(full) - base)
      ...> end
      iex> take_prefix.("Mr. John", "Mr. ")
      "John"

  Or simply use pattern matching:

      iex> take_prefix = fn full, prefix ->
      ...>   base = byte_size(prefix)
      ...>   <<_::binary-size(base), rest::binary>> = full
      ...>   rest
      ...> end
      iex> take_prefix.("Mr. John", "Mr. ")
      "John"

  On the other hand, if you want to dynamically slice a string
  based on an integer value, then using `String.slice/3` is the
  best option as it guarantees we won't incorrectly split a valid
  codepoint into multiple bytes.

  ## Integer codepoints

  Although codepoints could be represented as integers, this
  module represents all codepoints as strings. For example:

      iex> String.codepoints("olá")
      ["o", "l", "á"]

  There are a couple of ways to retrieve a character integer
  codepoint. One may use the `?` construct:

      iex> ?o
      111

      iex> ?á
      225

  Or also via pattern matching:

      iex> <<aacute::utf8>> = "á"
      iex> aacute
      225

  As we have seen above, codepoints can be inserted into
  a string by their hexadecimal code:

      "ol\u0061\u0301" #=>
      "olá"

  ## Self-synchronization

  The UTF-8 encoding is self-synchronizing. This means that
  if malformed data (i.e., data that is not possible according
  to the definition of the encoding) is encountered, only one
  codepoint needs to be rejected.

  This module relies on this behaviour to ignore such invalid
  characters. For example, `length/1` will return
  a correct result even if an invalid codepoint is fed into it.

  In other words, this module expects invalid data to be detected
  elsewhere, usually when retrieving data from the external source.
  For example, a driver that reads strings from a database will be
  responsible to check the validity of the encoding. `String.chunk/2`
  can be used for breaking a string into valid and invalid parts.

  ## Patterns

  Many functions in this module work with patterns. For example,
  String.split/2 can split a string into multiple patterns given
  a pattern. This pattern can be a string, a list of strings or
  a compiled pattern:

      iex> String.split("foo bar", " ")
      ["foo", "bar"]

      iex> String.split("foo bar!", [" ", "!"])
      ["foo", "bar", ""]

      iex> pattern = :binary.compile_pattern([" ", "!"])
      iex> String.split("foo bar!", pattern)
      ["foo", "bar", ""]

  The compiled pattern is useful when the same match will
  be done over and over again. Note though the compiled
  pattern cannot be stored in a module attribute as the pattern
  is generated at runtime and does not survive compile term.
  """

  @type t :: binary
  @type codepoint :: t
  @type grapheme :: t
  @type pattern :: t | [t] | :binary.cp

  @doc """
  Checks if a string contains only printable characters.

  ## Examples

      iex> String.printable?("abc")
      true

  """
  @spec printable?(t) :: boolean
  def printable?(string)

  for char <- 0x20..0x7E do
    def printable?(<<unquote(char), rest::binary>>) do
      printable?(rest)
    end
  end
  def printable?(<<?\n, rest::binary>>), do: printable?(rest)
  def printable?(<<?\r, rest::binary>>), do: printable?(rest)
  def printable?(<<?\t, rest::binary>>), do: printable?(rest)
  def printable?(<<?\v, rest::binary>>), do: printable?(rest)
  def printable?(<<?\b, rest::binary>>), do: printable?(rest)
  def printable?(<<?\f, rest::binary>>), do: printable?(rest)
  def printable?(<<?\e, rest::binary>>), do: printable?(rest)
  def printable?(<<?\d, rest::binary>>), do: printable?(rest)
  def printable?(<<?\a, rest::binary>>), do: printable?(rest)

  def printable?(<<h::utf8, t::binary>>)
      when h in 0xA0..0xD7FF
      when h in 0xE000..0xFFFD
      when h in 0x10000..0x10FFFF do
    printable?(t)
  end

  def printable?(<<>>), do: true
  def printable?(binary) when is_binary(binary), do: false

  @doc ~S"""
  Divides a string into substrings at each Unicode whitespace
  occurrence with leading and trailing whitespace ignored. Groups
  of whitespace are treated as a single occurrence. Divisions do
  not occur on non-breaking whitespace.

  ## Examples

      iex> String.split("foo bar")
      ["foo", "bar"]

      iex> String.split("foo" <> <<194, 133>> <> "bar")
      ["foo", "bar"]

      iex> String.split(" foo   bar")
      ["foo", "bar"]

      iex> String.split("no\u00a0break")
      ["no\u00a0break"]

  """
  @spec split(t) :: [t]
  defdelegate split(binary), to: String.Break

  @doc ~S"""
  Divides a string into substrings based on a pattern.

  Returns a list of these substrings. The pattern can
  be a string, a list of strings or a regular expression.

  The string is split into as many parts as possible by
  default, but can be controlled via the `parts: pos_integer` option.
  If you pass `parts: :infinity`, it will return all possible parts
  (`:infinity` is the default).

  Empty strings are only removed from the result if the
  `trim` option is set to `true` (default is `false`).

  When the pattern used is a regular expression, the string is
  split using `Regex.split/3`. In that case this function accepts
  additional options which are documented in `Regex.split/3`.

  ## Examples

  Splitting with a string pattern:

      iex> String.split("a,b,c", ",")
      ["a", "b", "c"]

      iex> String.split("a,b,c", ",", parts: 2)
      ["a", "b,c"]

      iex> String.split(" a b c ", " ", trim: true)
      ["a", "b", "c"]

  A list of patterns:

      iex> String.split("1,2 3,4", [" ", ","])
      ["1", "2", "3", "4"]

  A regular expression:

      iex> String.split("a,b,c", ~r{,})
      ["a", "b", "c"]

      iex> String.split("a,b,c", ~r{,}, parts: 2)
      ["a", "b,c"]

      iex> String.split(" a b c ", ~r{\s}, trim: true)
      ["a", "b", "c"]

      iex> String.split("abc", ~r{b}, include_captures: true)
      ["a", "b", "c"]

  Splitting on empty patterns returns graphemes:

      iex> String.split("abc", ~r{})
      ["a", "b", "c", ""]

      iex> String.split("abc", "")
      ["a", "b", "c", ""]

      iex> String.split("abc", "", trim: true)
      ["a", "b", "c"]

      iex> String.split("abc", "", parts: 2)
      ["a", "bc"]

  A precompiled pattern can also be given:

      iex> pattern = :binary.compile_pattern([" ", ","])
      iex> String.split("1,2 3,4", pattern)
      ["1", "2", "3", "4"]

  """
  @spec split(t, pattern | Regex.t) :: [t]
  @spec split(t, pattern | Regex.t, Keyword.t) :: [t]
  def split(string, pattern, options \\ [])

  def split(string, %Regex{} = pattern, options) when is_binary(string) do
    Regex.split(pattern, string, options)
  end

  def split(string, pattern, []) when is_binary(string) and pattern != "" do
    :binary.split(string, pattern, [:global])
  end

  def split(string, pattern, options) when is_binary(string) do
    parts   = Keyword.get(options, :parts, :infinity)
    trim    = Keyword.get(options, :trim, false)
    pattern = maybe_compile_pattern(pattern)
    split_each(string, pattern, trim, parts_to_index(parts))
  end

  defp parts_to_index(:infinity), do: 0
  defp parts_to_index(n) when is_integer(n) and n > 0, do: n

  defp split_each("", _pattern, true, 1), do: []
  defp split_each(string, _pattern, _trim, 1) when is_binary(string), do: [string]
  defp split_each(string, pattern, trim, count) do
    case do_splitter(string, pattern, trim) do
      {h, t} -> [h | split_each(t, pattern, trim, count -1)]
      nil    -> []
    end
  end

  defp do_splitter(:nomatch, _pattern, _), do: nil
  defp do_splitter("", _pattern, true),    do: nil
  defp do_splitter("", _pattern, false),   do: {"", :nomatch}

  defp do_splitter(bin, "", _trim) do
    next_grapheme(bin)
  end

  defp do_splitter(bin, pattern, trim) do
    case :binary.match(bin, pattern) do
      {0, length} when trim ->
        do_splitter(:binary.part(bin, length, byte_size(bin) - length), pattern, trim)
      {pos, length} ->
        final = pos + length
        {:binary.part(bin, 0, pos),
         :binary.part(bin, final, byte_size(bin) - final)}
      :nomatch ->
        {bin, :nomatch}
    end
  end

  defp maybe_compile_pattern(""), do: ""
  defp maybe_compile_pattern(pattern) when is_tuple(pattern), do: pattern
  defp maybe_compile_pattern(pattern), do: :binary.compile_pattern(pattern)

  @doc """
  Returns the next grapheme in a string.

  The result is a tuple with the grapheme and the
  remainder of the string or `nil` in case
  the String reached its end.

  ## Examples

      iex> String.next_grapheme("olá")
      {"o", "lá"}
  """
  @spec next_grapheme(t) :: {grapheme, t} | nil
  def next_grapheme(binary) do
    case next_grapheme_size(binary) do
      {size, rest} -> {:binary.part(binary, 0, size), rest}
      nil          -> nil
    end
  end

  @doc """
  Returns the size of the next grapheme.

  The result is a tuple with the next grapheme size and
  the remainder of the string or `nil` in case the string
  reached its end.

  ## Examples

      iex> String.next_grapheme_size("olá")
      {1, "lá"}

  """
  @spec next_grapheme_size(t) :: {pos_integer, t} | nil
  defdelegate next_grapheme_size(string), to: String.Unicode
end
