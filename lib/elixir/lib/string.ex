import Kernel, except: [length: 1]

defmodule String do
  @moduledoc ~S"""
  A String in Elixir is a UTF-8 encoded binary.

  ## Codepoints and grapheme cluster

  The functions in this module act according to the Unicode
  Standard, version 10.0.0.

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

  For converting a binary to a different encoding and for Unicode
  normalization mechanisms, see Erlang's `:unicode` module.

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
  `String.split/2` can split a string into multiple patterns given
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
  @type pattern :: t | [t] | :binary.cp()

  @conditional_mappings [:greek]

  @doc """
  Checks if a string contains only printable characters.

  Takes an optional `limit` as a second argument. `printable?/2` only
  checks the printability of the string up to the `limit`.

  ## Examples

      iex> String.printable?("abc")
      true

      iex> String.printable?("abc" <> <<0>>)
      false

      iex> String.printable?("abc" <> <<0>>, 2)
      true

  """
  @spec printable?(t) :: boolean
  @spec printable?(t, non_neg_integer | :infinity) :: boolean
  def printable?(string, counter \\ :infinity)

  def printable?(<<>>, _), do: true
  def printable?(_, 0), do: true

  for char <- 0x20..0x7E do
    def printable?(<<unquote(char), rest::binary>>, counter) do
      printable?(rest, decrement(counter))
    end
  end

  for char <- '\n\r\t\v\b\f\e\d\a' do
    def printable?(<<unquote(char), rest::binary>>, counter) do
      printable?(rest, decrement(counter))
    end
  end

  def printable?(<<char::utf8, rest::binary>>, counter)
      when char in 0xA0..0xD7FF
      when char in 0xE000..0xFFFD
      when char in 0x10000..0x10FFFF do
    printable?(rest, decrement(counter))
  end

  def printable?(binary, _) when is_binary(binary), do: false

  defp decrement(:infinity), do: :infinity
  defp decrement(counter), do: counter - 1

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

      iex> String.split(" foo   bar ")
      ["foo", "bar"]

      iex> String.split("no\u00a0break")
      ["no\u00a0break"]

  """
  @spec split(t) :: [t]
  defdelegate split(binary), to: String.Break

  @doc ~S"""
  Divides a string into substrings based on a pattern.

  Returns a list of these substrings. The pattern can
  be a string, a list of strings, or a regular expression.

  The string is split into as many parts as possible by
  default, but can be controlled via the `:parts` option.

  Empty strings are only removed from the result if the
  `:trim` option is set to `true`.

  When the pattern used is a regular expression, the string is
  split using `Regex.split/3`.

  ## Options

    * `:parts` (positive integer or `:infinity`) - the string
      is split into at most as many parts as this option specifies.
      If `:infinity`, the string will be split into all possible
      parts. Defaults to `:infinity`.

    * `:trim` (boolean) - if `true`, empty strings are removed from
      the resulting list.

  This function also accepts all options accepted by `Regex.split/3`
  if `pattern` is a regular expression.

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

  Splitting on empty string returns graphemes:

      iex> String.split("abc", "")
      ["", "a", "b", "c", ""]

      iex> String.split("abc", "", trim: true)
      ["a", "b", "c"]

      iex> String.split("abc", "", parts: 1)
      ["abc"]

      iex> String.split("abc", "", parts: 3)
      ["", "a", "bc"]

  A precompiled pattern can also be given:

      iex> pattern = :binary.compile_pattern([" ", ","])
      iex> String.split("1,2 3,4", pattern)
      ["1", "2", "3", "4"]

  Note this function can split within or across grapheme boundaries.
  For example, take the grapheme "é" which is made of the characters
  "e" and the acute accent. The following returns true:

      iex> String.split(String.normalize("é", :nfd), "e")
      ["", "́"]

  However, if "é" is represented by the single character "e with acute"
  accent, then it will return false:

      iex> String.split(String.normalize("é", :nfc), "e")
      ["é"]

  """
  @spec split(t, pattern | Regex.t(), keyword) :: [t]
  def split(string, pattern, options \\ [])

  def split(string, %Regex{} = pattern, options) when is_binary(string) do
    Regex.split(pattern, string, options)
  end

  def split(string, "", options) when is_binary(string) do
    parts = Keyword.get(options, :parts, :infinity)
    index = parts_to_index(parts)
    trim = Keyword.get(options, :trim, false)

    if trim == false and index != 1 do
      ["" | split_empty(string, trim, index - 1)]
    else
      split_empty(string, trim, index)
    end
  end

  def split(string, pattern, []) when is_tuple(pattern) or is_binary(string) do
    :binary.split(string, pattern, [:global])
  end

  def split(string, pattern, options) when is_binary(string) do
    parts = Keyword.get(options, :parts, :infinity)
    trim = Keyword.get(options, :trim, false)
    pattern = maybe_compile_pattern(pattern)
    split_each(string, pattern, trim, parts_to_index(parts))
  end

  defp parts_to_index(:infinity), do: 0
  defp parts_to_index(n) when is_integer(n) and n > 0, do: n

  defp split_empty("", true, 1), do: []
  defp split_empty(string, _, 1), do: [string]

  defp split_empty(string, trim, count) do
    case next_grapheme(string) do
      {h, t} -> [h | split_empty(t, trim, count - 1)]
      nil -> split_empty("", trim, 1)
    end
  end

  defp split_each("", _pattern, true, 1), do: []
  defp split_each(string, _pattern, _trim, 1) when is_binary(string), do: [string]

  defp split_each(string, pattern, trim, count) do
    case do_splitter(string, pattern, trim) do
      {h, t} -> [h | split_each(t, pattern, trim, count - 1)]
      nil -> []
    end
  end

  @doc """
  Returns an enumerable that splits a string on demand.

  This is in contrast to `split/3` which splits all
  the string upfront.

  Note splitter does not support regular expressions
  (as it is often more efficient to have the regular
  expressions traverse the string at once than in
  multiple passes).

  ## Options

    * :trim - when `true`, does not emit empty patterns

  ## Examples

      iex> String.splitter("1,2 3,4 5,6 7,8,...,99999", [" ", ","]) |> Enum.take(4)
      ["1", "2", "3", "4"]

      iex> String.splitter("abcd", "") |> Enum.take(10)
      ["", "a", "b", "c", "d", ""]

      iex> String.splitter("abcd", "", trim: true) |> Enum.take(10)
      ["a", "b", "c", "d"]

  """
  @spec splitter(t, pattern, keyword) :: Enumerable.t()
  def splitter(string, pattern, options \\ [])

  def splitter(string, "", options) do
    if Keyword.get(options, :trim, false) do
      Stream.unfold(string, &next_grapheme/1)
    else
      Stream.unfold(:match, &do_empty_splitter(&1, string))
    end
  end

  def splitter(string, pattern, options) do
    pattern = maybe_compile_pattern(pattern)
    trim = Keyword.get(options, :trim, false)
    Stream.unfold(string, &do_splitter(&1, pattern, trim))
  end

  defp do_empty_splitter(:match, string), do: {"", string}
  defp do_empty_splitter(:nomatch, _string), do: nil
  defp do_empty_splitter("", _), do: {"", :nomatch}
  defp do_empty_splitter(string, _), do: next_grapheme(string)

  defp do_splitter(:nomatch, _pattern, _), do: nil
  defp do_splitter("", _pattern, false), do: {"", :nomatch}
  defp do_splitter("", _pattern, true), do: nil

  defp do_splitter(bin, pattern, trim) do
    case :binary.split(bin, pattern) do
      ["", second] when trim -> do_splitter(second, pattern, trim)
      [first, second] -> {first, second}
      [first] -> {first, :nomatch}
    end
  end

  defp maybe_compile_pattern(pattern) when is_tuple(pattern), do: pattern
  defp maybe_compile_pattern(pattern), do: :binary.compile_pattern(pattern)

  @doc """
  Splits a string into two at the specified offset. When the offset given is
  negative, location is counted from the end of the string.

  The offset is capped to the length of the string. Returns a tuple with
  two elements.

  Note: keep in mind this function splits on graphemes and for such it
  has to linearly traverse the string. If you want to split a string or
  a binary based on the number of bytes, use `Kernel.binary_part/3`
  instead.

  ## Examples

      iex> String.split_at "sweetelixir", 5
      {"sweet", "elixir"}

      iex> String.split_at "sweetelixir", -6
      {"sweet", "elixir"}

      iex> String.split_at "abc", 0
      {"", "abc"}

      iex> String.split_at "abc", 1000
      {"abc", ""}

      iex> String.split_at "abc", -1000
      {"", "abc"}

  """
  @spec split_at(t, integer) :: {t, t}
  def split_at(string, position)

  def split_at(string, position) when is_integer(position) and position >= 0 do
    do_split_at(string, position)
  end

  def split_at(string, position) when is_integer(position) and position < 0 do
    position = length(string) + position

    case position >= 0 do
      true -> do_split_at(string, position)
      false -> {"", string}
    end
  end

  defp do_split_at(string, position) do
    {byte_size, rest} = String.Unicode.split_at(string, position)
    {binary_part(string, 0, byte_size), rest || ""}
  end

  @doc ~S"""
  Returns `true` if `string1` is canonically equivalent to 'string2'.

  It performs Normalization Form Canonical Decomposition (NFD) on the
  strings before comparing them. This function is equivalent to:

      String.normalize(string1, :nfd) == String.normalize(string2, :nfd)

  Therefore, if you plan to compare multiple strings, multiple times
  in a row, you may normalize them upfront and compare them directly
  to avoid multiple normalization passes.

  ## Examples

      iex> String.equivalent?("abc", "abc")
      true

      iex> String.equivalent?("man\u0303ana", "mañana")
      true

      iex> String.equivalent?("abc", "ABC")
      false

      iex> String.equivalent?("nø", "nó")
      false

  """
  @spec equivalent?(t, t) :: boolean
  def equivalent?(string1, string2) do
    normalize(string1, :nfd) == normalize(string2, :nfd)
  end

  @doc """
  Converts all characters in `string` to Unicode normalization
  form identified by `form`.

  ## Forms

  The supported forms are:

    * `:nfd` - Normalization Form Canonical Decomposition.
      Characters are decomposed by canonical equivalence, and
      multiple combining characters are arranged in a specific
      order.

    * `:nfc` - Normalization Form Canonical Composition.
      Characters are decomposed and then recomposed by canonical equivalence.

  ## Examples

      iex> String.normalize("yêṩ", :nfd)
      "yêṩ"

      iex> String.normalize("leña", :nfc)
      "leña"

  """
  @spec normalize(t, atom) :: t
  defdelegate normalize(string, form), to: String.Normalizer

  @doc """
  Converts all characters in the given string to uppercase according to `mode`.

  `mode` may be `:default`, `:ascii` or `:greek`. The `:default` mode considers
  all non-conditional transformations outlined in the Unicode standard. `:ascii`
  uppercases only the letters a to z. `:greek` includes the context sensitive
  mappings found in Greek.

  ## Examples

      iex> String.upcase("abcd")
      "ABCD"

      iex> String.upcase("ab 123 xpto")
      "AB 123 XPTO"

      iex> String.upcase("olá")
      "OLÁ"

  The `:ascii` mode ignores Unicode characters and provides a more
  performant implementation when you know the string contains only
  ASCII characters:

      iex> String.upcase("olá", :ascii)
      "OLá"

  """
  @spec upcase(t, :default | :ascii | :greek) :: t
  def upcase(string, mode \\ :default)

  def upcase("", _mode) do
    ""
  end

  def upcase(string, :default) when is_binary(string) do
    String.Casing.upcase(string, [], :default)
  end

  def upcase(string, :ascii) when is_binary(string) do
    for <<x <- string>>,
      do: if(x >= ?a and x <= ?z, do: <<x - 32>>, else: <<x>>),
      into: ""
  end

  def upcase(string, mode) when mode in @conditional_mappings do
    String.Casing.upcase(string, [], mode)
  end

  @doc """
  Converts all characters in the given string to lowercase according to `mode`.

  `mode` may be `:default`, `:ascii` or `:greek`. The `:default` mode considers
  all non-conditional transformations outlined in the Unicode standard. `:ascii`
  lowercases only the letters A to Z. `:greek` includes the context sensitive
  mappings found in Greek.

  ## Examples

      iex> String.downcase("ABCD")
      "abcd"

      iex> String.downcase("AB 123 XPTO")
      "ab 123 xpto"

      iex> String.downcase("OLÁ")
      "olá"

  The `:ascii` mode ignores Unicode characters and provides a more
  performant implementation when you know the string contains only
  ASCII characters:

      iex> String.downcase("OLÁ", :ascii)
      "olÁ"

  And `:greek` properly handles the context sensitive sigma in Greek:

      iex> String.downcase("ΣΣ")
      "σσ"

      iex> String.downcase("ΣΣ", :greek)
      "σς"

  """
  @spec downcase(t, :default | :ascii | :greek) :: t
  def downcase(string, mode \\ :default)

  def downcase("", _mode) do
    ""
  end

  def downcase(string, :default) when is_binary(string) do
    String.Casing.downcase(string, [], :default)
  end

  def downcase(string, :ascii) when is_binary(string) do
    for <<x <- string>>,
      do: if(x >= ?A and x <= ?Z, do: <<x + 32>>, else: <<x>>),
      into: ""
  end

  def downcase(string, mode) when mode in @conditional_mappings do
    String.Casing.downcase(string, [], mode)
  end

  @doc """
  Converts the first character in the given string to
  uppercase and the remainder to lowercase according to `mode`.

  `mode` may be `:default`, `:ascii` or `:greek`. The `:default` mode considers
  all non-conditional transformations outlined in the Unicode standard. `:ascii`
  lowercases only the letters A to Z. `:greek` includes the context sensitive
  mappings found in Greek.

  ## Examples

      iex> String.capitalize("abcd")
      "Abcd"

      iex> String.capitalize("ﬁn")
      "Fin"

      iex> String.capitalize("olá")
      "Olá"

  """
  @spec capitalize(t, :default | :ascii | :greek) :: t
  def capitalize(string, mode \\ :default)

  def capitalize(<<char, rest::binary>>, :ascii) do
    char = if char >= ?a and char <= ?z, do: char - 32, else: char
    <<char>> <> downcase(rest, :ascii)
  end

  def capitalize(string, mode) when is_binary(string) do
    {char, rest} = String.Casing.titlecase_once(string, mode)
    char <> downcase(rest, mode)
  end

  @doc false
  # TODO: Remove by 2.0
  # (hard-deprecated in elixir_dispatch)
  defdelegate rstrip(binary), to: String.Break, as: :trim_trailing

  @doc false
  # TODO: Remove by 2.0
  # (hard-deprecated in elixir_dispatch)
  def rstrip(string, char) when is_integer(char) do
    replace_trailing(string, <<char::utf8>>, "")
  end

  @doc """
  Replaces all leading occurrences of `match` by `replacement` of `match` in `string`.

  Returns the string untouched if there are no occurrences.

  If `match` is `""`, this function raises an `ArgumentError` exception: this
  happens because this function replaces **all** the occurrences of `match` at
  the beginning of `string`, and it's impossible to replace "multiple"
  occurrences of `""`.

  ## Examples

      iex> String.replace_leading("hello world", "hello ", "")
      "world"
      iex> String.replace_leading("hello hello world", "hello ", "")
      "world"

      iex> String.replace_leading("hello world", "hello ", "ola ")
      "ola world"
      iex> String.replace_leading("hello hello world", "hello ", "ola ")
      "ola ola world"

  """
  @spec replace_leading(t, t, t) :: t | no_return
  def replace_leading(string, match, replacement)
      when is_binary(string) and is_binary(match) and is_binary(replacement) do
    if match == "" do
      raise ArgumentError, "cannot use an empty string as the match to replace"
    end

    prefix_size = byte_size(match)
    suffix_size = byte_size(string) - prefix_size
    replace_leading(string, match, replacement, prefix_size, suffix_size, 0)
  end

  defp replace_leading(string, match, replacement, prefix_size, suffix_size, acc)
       when suffix_size >= 0 do
    case string do
      <<prefix::size(prefix_size)-binary, suffix::binary>> when prefix == match ->
        replace_leading(
          suffix,
          match,
          replacement,
          prefix_size,
          suffix_size - prefix_size,
          acc + 1
        )

      _ ->
        prepend_unless_empty(duplicate(replacement, acc), string)
    end
  end

  defp replace_leading(string, _match, replacement, _prefix_size, _suffix_size, acc) do
    prepend_unless_empty(duplicate(replacement, acc), string)
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
