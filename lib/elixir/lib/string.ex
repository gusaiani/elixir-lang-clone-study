import Kernel, except: [length: 1]

defmodule String do
  @moduledoc ~S"""
  A String in Elixir is a UTF-8 encoded binary.

  ## Codepoints and grapheme cluster

  The functions in this module act according to the Unicode
  Standard, version 11.0.0.

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

  @typedoc """
  A UTF-8 encoded binary.

  Note `String.t()` and `binary()` are equivalent to analysis tools.
  Although, for those reading the documentation, `String.t()` implies
  it is a UTF-8 encoded binary.
  """
  @type t :: binary

  @typedoc "A UTF-8 codepoint. It may be one or more bytes."
  @type codepoint :: t

  @typedoc "Multiple codepoints that may be perceived as a single character by readers"
  @type grapheme :: t

  @typedoc "Pattern used in functions like `replace/3` and `split/2`"
  @type pattern :: t | [t] | :binary.cp()

  @conditional_mappings [:greek]

  @doc """
  Checks if a string contains only printable characters up to `character_limit`.

  Takes an optional `character_limit` as a second argument. If `character_limit` is `0`, this
  function will return `true`.

  ## Examples

      iex> String.printable?("abc")
      true

      iex> String.printable?("abc" <> <<0>>)
      false

      iex> String.printable?("abc" <> <<0>>, 2)
      true

      iex> String.printable?("abc" <> <<0>>, 0)
      true

  """
  @spec printable?(t, 0) :: true
  @spec printable?(t, pos_integer | :infinity) :: boolean
  def printable?(string, character_limit \\ :infinity)
      when is_binary(string) and
             (character_limit == :infinity or
                (is_integer(character_limit) and character_limit >= 0)) do
    recur_printable?(string, character_limit)
  end

  defp recur_printable?(_string, 0), do: true
  defp recur_printable?(<<>>, _character_limit), do: true

  for char <- 0x20..0x7E do
    defp recur_printable?(<<unquote(char), rest::binary>>, character_limit) do
      recur_printable?(rest, decrement(character_limit))
    end
  end

  for char <- '\n\r\t\v\b\f\e\d\a' do
    defp recur_printable?(<<unquote(char), rest::binary>>, character_limit) do
      recur_printable?(rest, decrement(character_limit))
    end
  end

  defp recur_printable?(<<char::utf8, rest::binary>>, character_limit)
       when char in 0xA0..0xD7FF
       when char in 0xE000..0xFFFD
       when char in 0x10000..0x10FFFF do
    recur_printable?(rest, decrement(character_limit))
  end

  defp recur_printable?(_string, _character_limit) do
    false
  end

  defp decrement(:infinity), do: :infinity
  defp decrement(character_limit), do: character_limit - 1

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
  be a string, a list of strings, a regular expression,
  or a compiled pattern.

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

  A compiled pattern:

      iex> pattern = :binary.compile_pattern([" ", ","])
      iex> String.split("1,2 3,4", pattern)
      ["1", "2", "3", "4"]

  Splitting on empty string returns graphemes:

      iex> String.split("abc", "")
      ["", "a", "b", "c", ""]

      iex> String.split("abc", "", trim: true)
      ["a", "b", "c"]

      iex> String.split("abc", "", parts: 1)
      ["abc"]

      iex> String.split("abc", "", parts: 3)
      ["", "a", "bc"]

  Note this function can split within or across grapheme boundaries.
  For example, take the grapheme "é" which is made of the characters
  "e" and the acute accent. The following returns `true`:

      iex> String.split(String.normalize("é", :nfd), "e")
      ["", "́"]

  However, if "é" is represented by the single character "e with acute"
  accent, then it will return `false`:

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

  This is in contrast to `split/3` which splits the
  entire string upfront.

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

  A compiled pattern can also be given:

      iex> pattern = :binary.compile_pattern([" ", ","])
      iex> String.splitter("1,2 3,4 5,6 7,8,...,99999", pattern) |> Enum.take(4)
      ["1", "2", "3", "4"]

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

      iex> String.split_at("sweetelixir", 5)
      {"sweet", "elixir"}

      iex> String.split_at("sweetelixir", -6)
      {"sweet", "elixir"}

      iex> String.split_at("abc", 0)
      {"", "abc"}

      iex> String.split_at("abc", 1000)
      {"abc", ""}

      iex> String.split_at("abc", -1000)
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
  # TODO: Fully deprecate it on v1.10
  @doc deprecated:
         "Use :unicode.characters_to_nfc_binary/1 or :unicode.characters_to_nfd_binary/1 instead"
  def normalize(string, form)

  def normalize(string, :nfd) do
    case :unicode.characters_to_nfd_binary(string) do
      string when is_binary(string) -> string
      {:error, bad, rest} -> bad <> normalize(rest, :nfd)
    end
  end

  def normalize(string, :nfc) do
    case :unicode.characters_to_nfc_binary(string) do
      string when is_binary(string) -> string
      {:error, bad, rest} -> bad <> normalize(rest, :nfc)
    end
  end

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
    IO.iodata_to_binary(upcase_ascii(string))
  end

  def upcase(string, mode) when mode in @conditional_mappings do
    String.Casing.upcase(string, [], mode)
  end

  defp upcase_ascii(<<char, rest::bits>>) when char >= ?a and char <= ?z,
    do: [char - 32 | upcase_ascii(rest)]

  defp upcase_ascii(<<char, rest::bits>>), do: [char | upcase_ascii(rest)]
  defp upcase_ascii(<<>>), do: []

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
    IO.iodata_to_binary(downcase_ascii(string))
  end

  def downcase(string, mode) when mode in @conditional_mappings do
    String.Casing.downcase(string, [], mode)
  end

  defp downcase_ascii(<<char, rest::bits>>) when char >= ?A and char <= ?Z,
    do: [char + 32 | downcase_ascii(rest)]

  defp downcase_ascii(<<char, rest::bits>>), do: [char | downcase_ascii(rest)]
  defp downcase_ascii(<<>>), do: []

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
  @deprecated "Use String.trim_trailing/1 instead"
  defdelegate rstrip(binary), to: String.Break, as: :trim_trailing

  @doc false
  # TODO: Remove by 2.0
  @deprecated "Use String.trim_trailing/2 with a binary as second argument instead"
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

  @doc """
  Replaces all trailing occurrences of `match` by `replacement` in `string`.

  Returns the string untouched if there are no occurrences.

  If `match` is `""`, this function raises an `ArgumentError` exception: this
  happens because this function replaces **all** the occurrences of `match` at
  the end of `string`, and it's impossible to replace "multiple" occurrences of
  `""`.

  ## Examples

      iex> String.replace_trailing("hello world", " world", "")
      "hello"
      iex> String.replace_trailing("hello world world", " world", "")
      "hello"

      iex> String.replace_trailing("hello world", " world", " mundo")
      "hello mundo"
      iex> String.replace_trailing("hello world world", " world", " mundo")
      "hello mundo mundo"

  """
  @spec replace_trailing(t, t, t) :: t | no_return
  def replace_trailing(string, match, replacement)
      when is_binary(string) and is_binary(match) and is_binary(replacement) do
    if match == "" do
      raise ArgumentError, "cannot use an empty string as the match to replace"
    end

    suffix_size = byte_size(match)
    prefix_size = byte_size(string) - suffix_size
    replace_trailing(string, match, replacement, prefix_size, suffix_size, 0)
  end

  defp replace_trailing(string, match, replacement, prefix_size, suffix_size, acc)
       when prefix_size >= 0 do
    case string do
      <<prefix::size(prefix_size)-binary, suffix::binary>> when suffix == match ->
        replace_trailing(
          prefix,
          match,
          replacement,
          prefix_size - suffix_size,
          suffix_size,
          acc + 1
        )

      _ ->
        append_unless_empty(string, duplicate(replacement, acc))
    end
  end

  defp replace_trailing(string, _match, replacement, _prefix_size, _suffix_size, acc) do
    append_unless_empty(string, duplicate(replacement, acc))
  end

  @doc """
  Replaces prefix in `string` by `replacement` if it matches `match`.

  Returns the string untouched if there is no match. If `match` is an empty
  string (`""`), `replacement` is just prepended to `string`.

  ## Examples

      iex> String.replace_prefix("world", "hello ", "")
      "world"
      iex> String.replace_prefix("hello world", "hello ", "")
      "world"
      iex> String.replace_prefix("hello hello world", "hello ", "")
      "hello world"

      iex> String.replace_prefix("world", "hello ", "ola ")
      "world"
      iex> String.replace_prefix("hello world", "hello ", "ola ")
      "ola world"
      iex> String.replace_prefix("hello hello world", "hello ", "ola ")
      "ola hello world"

      iex> String.replace_prefix("world", "", "hello ")
      "hello world"

  """
  @spec replace_prefix(t, t, t) :: t
  def replace_prefix(string, match, replacement)
      when is_binary(string) and is_binary(match) and is_binary(replacement) do
    prefix_size = byte_size(match)

    case string do
      <<prefix::size(prefix_size)-binary, suffix::binary>> when prefix == match ->
        prepend_unless_empty(replacement, suffix)

      _ ->
        string
    end
  end

  @doc """
  Replaces suffix in `string` by `replacement` if it matches `match`.

  Returns the string untouched if there is no match. If `match` is an empty
  string (`""`), `replacement` is just appended to `string`.

  ## Examples

      iex> String.replace_suffix("hello", " world", "")
      "hello"
      iex> String.replace_suffix("hello world", " world", "")
      "hello"
      iex> String.replace_suffix("hello world world", " world", "")
      "hello world"

      iex> String.replace_suffix("hello", " world", " mundo")
      "hello"
      iex> String.replace_suffix("hello world", " world", " mundo")
      "hello mundo"
      iex> String.replace_suffix("hello world world", " world", " mundo")
      "hello world mundo"

      iex> String.replace_suffix("hello", "", " world")
      "hello world"

  """
  @spec replace_suffix(t, t, t) :: t
  def replace_suffix(string, match, replacement)
      when is_binary(string) and is_binary(match) and is_binary(replacement) do
    suffix_size = byte_size(match)
    prefix_size = byte_size(string) - suffix_size

    case string do
      <<prefix::size(prefix_size)-binary, suffix::binary>> when suffix == match ->
        append_unless_empty(prefix, replacement)

      _ ->
        string
    end
  end

  @compile {:inline, prepend_unless_empty: 2, append_unless_empty: 2}

  defp prepend_unless_empty("", suffix), do: suffix
  defp prepend_unless_empty(prefix, suffix), do: prefix <> suffix

  defp append_unless_empty(prefix, ""), do: prefix
  defp append_unless_empty(prefix, suffix), do: prefix <> suffix

  @doc false
  # TODO: Remove by 2.0
  @deprecated "Use String.trim_leading/1 instead"
  defdelegate lstrip(binary), to: String.Break, as: :trim_leading

  @doc false
  # TODO: Remove by 2.0
  @deprecated "Use String.trim_leading/2 with a binary as second argument instead"
  def lstrip(string, char) when is_integer(char) do
    replace_leading(string, <<char::utf8>>, "")
  end

  @doc false
  # TODO: Remove by 2.0
  @deprecated "Use String.trim/1 instead"
  def strip(string) do
    trim(string)
  end

  @doc false
  # TODO: Remove by 2.0
  @deprecated "Use String.trim/2 with a binary second argument instead"
  def strip(string, char) do
    trim(string, <<char::utf8>>)
  end

  @doc ~S"""
  Returns a string where all leading Unicode whitespaces
  have been removed.

  ## Examples

      iex> String.trim_leading("\n  abc   ")
      "abc   "

  """
  @spec trim_leading(t) :: t
  defdelegate trim_leading(string), to: String.Break

  @doc """
  Returns a string where all leading `to_trim`s have been removed.

  ## Examples

      iex> String.trim_leading("__ abc _", "_")
      " abc _"

      iex> String.trim_leading("1 abc", "11")
      "1 abc"

  """
  @spec trim_leading(t, t) :: t
  def trim_leading(string, to_trim) do
    replace_leading(string, to_trim, "")
  end

  @doc ~S"""
  Returns a string where all trailing Unicode whitespaces
  has been removed.

  ## Examples

      iex> String.trim_trailing("   abc\n  ")
      "   abc"

  """
  @spec trim_trailing(t) :: t
  defdelegate trim_trailing(string), to: String.Break

  @doc """
  Returns a string where all trailing `to_trim`s have been removed.

  ## Examples

      iex> String.trim_trailing("_ abc __", "_")
      "_ abc "

      iex> String.trim_trailing("abc 1", "11")
      "abc 1"

  """
  @spec trim_trailing(t, t) :: t
  def trim_trailing(string, to_trim) do
    replace_trailing(string, to_trim, "")
  end

  @doc ~S"""
  Returns a string where all leading and trailing Unicode whitespaces
  have been removed.

  ## Examples

      iex> String.trim("\n  abc\n  ")
      "abc"

  """
  @spec trim(t) :: t
  def trim(string) do
    string
    |> trim_leading()
    |> trim_trailing()
  end

  @doc """
  Returns a string where all leading and trailing `to_trim`s have been
  removed.

  ## Examples

      iex> String.trim("a  abc  a", "a")
      "  abc  "

  """
  @spec trim(t, t) :: t
  def trim(string, to_trim) do
    string
    |> trim_leading(to_trim)
    |> trim_trailing(to_trim)
  end

  @doc ~S"""
  Returns a new string padded with a leading filler
  which is made of elements from the `padding`.

  Passing a list of strings as `padding` will take one element of the list
  for every missing entry. If the list is shorter than the number of inserts,
  the filling will start again from the beginning of the list.
  Passing a string `padding` is equivalent to passing the list of graphemes in it.
  If no `padding` is given, it defaults to whitespace.

  When `count` is less than or equal to the length of `string`,
  given `string` is returned.

  Raises `ArgumentError` if the given `padding` contains a non-string element.

  ## Examples

      iex> String.pad_leading("abc", 5)
      "  abc"

      iex> String.pad_leading("abc", 4, "12")
      "1abc"

      iex> String.pad_leading("abc", 6, "12")
      "121abc"

      iex> String.pad_leading("abc", 5, ["1", "23"])
      "123abc"

  """
  @spec pad_leading(t, non_neg_integer, t | [t]) :: t
  def pad_leading(string, count, padding \\ [" "])

  def pad_leading(string, count, padding) when is_binary(padding) do
    pad_leading(string, count, graphemes(padding))
  end

  def pad_leading(string, count, [_ | _] = padding)
      when is_binary(string) and is_integer(count) and count >= 0 do
    pad(:leading, string, count, padding)
  end

  @doc ~S"""
  Returns a new string padded with a trailing filler
  which is made of elements from the `padding`.

  Passing a list of strings as `padding` will take one element of the list
  for every missing entry. If the list is shorter than the number of inserts,
  the filling will start again from the beginning of the list.
  Passing a string `padding` is equivalent to passing the list of graphemes in it.
  If no `padding` is given, it defaults to whitespace.

  When `count` is less than or equal to the length of `string`,
  given `string` is returned.

  Raises `ArgumentError` if the given `padding` contains a non-string element.

  ## Examples

      iex> String.pad_trailing("abc", 5)
      "abc  "

      iex> String.pad_trailing("abc", 4, "12")
      "abc1"

      iex> String.pad_trailing("abc", 6, "12")
      "abc121"

      iex> String.pad_trailing("abc", 5, ["1", "23"])
      "abc123"

  """
  @spec pad_trailing(t, non_neg_integer, t | [t]) :: t
  def pad_trailing(string, count, padding \\ [" "])

  def pad_trailing(string, count, padding) when is_binary(padding) do
    pad_trailing(string, count, graphemes(padding))
  end

  def pad_trailing(string, count, [_ | _] = padding)
      when is_binary(string) and is_integer(count) and count >= 0 do
    pad(:trailing, string, count, padding)
  end

  defp pad(kind, string, count, padding) do
    string_len = length(string)

    if string_len >= count do
      string
    else
      filler = build_filler(count - string_len, padding, padding, 0, [])

      case kind do
        :leading -> [filler | string]
        :trailing -> [string | filler]
      end
      |> IO.iodata_to_binary()
    end
  end

  defp build_filler(0, _source, _padding, _size, filler), do: filler

  defp build_filler(count, source, [], size, filler) do
    rem_filler =
      rem(count, size)
      |> build_filler(source, source, 0, [])

    filler =
      filler
      |> IO.iodata_to_binary()
      |> duplicate(div(count, size) + 1)

    [filler | rem_filler]
  end

  defp build_filler(count, source, [elem | rest], size, filler)
       when is_binary(elem) do
    build_filler(count - 1, source, rest, size + 1, [filler | elem])
  end

  defp build_filler(_count, _source, [elem | _rest], _size, _filler) do
    raise ArgumentError, "expected a string padding element, got: #{inspect(elem)}"
  end

  @doc false
  # TODO: Remove by 2.0
  @deprecated "Use String.pad_leading/2 instead"
  def rjust(subject, len) do
    rjust(subject, len, ?\s)
  end

  @doc false
  # TODO: Remove by 2.0
  @deprecated "Use String.pad_leading/3 with a binary padding instead"
  def rjust(subject, len, pad) when is_integer(pad) and is_integer(len) and len >= 0 do
    pad(:leading, subject, len, [<<pad::utf8>>])
  end

  @doc false
  # TODO: Remove by 2.0
  @deprecated "Use String.pad_trailing/2 instead"
  def ljust(subject, len) do
    ljust(subject, len, ?\s)
  end

  @doc false
  # TODO: Remove by 2.0
  @deprecated "Use String.pad_trailing/3 with a binary padding instead"
  def ljust(subject, len, pad) when is_integer(pad) and is_integer(len) and len >= 0 do
    pad(:trailing, subject, len, [<<pad::utf8>>])
  end

  @doc ~S"""
  Returns a new string created by replacing occurrences of `pattern` in
  `subject` with `replacement`.

  The `pattern` may be a string, a regular expression, or a compiled pattern.

  By default it replaces all occurrences but this behaviour can be controlled
  through the `:global` option; see the "Options" section below.

  ## Options

    * `:global` - (boolean) if `true`, all occurrences of `pattern` are replaced
      with `replacement`, otherwise only the first occurrence is
      replaced. Defaults to `true`

    * `:insert_replaced` - (integer or list of integers) specifies the position
      where to insert the replaced part inside the `replacement`. If any
      position given in the `:insert_replaced` option is larger than the
      replacement string, or is negative, an `ArgumentError` is raised. See the
      examples below

  ## Examples

      iex> String.replace("a,b,c", ",", "-")
      "a-b-c"

      iex> String.replace("a,b,c", ",", "-", global: false)
      "a-b,c"

  When the pattern is a regular expression, one can give `\N` or
  `\g{N}` in the `replacement` string to access a specific capture in the
  regular expression:

      iex> String.replace("a,b,c", ~r/,(.)/, ",\\1\\g{1}")
      "a,bb,cc"

  Notice we had to escape the backslash escape character (i.e., we used `\\N`
  instead of just `\N` to escape the backslash; same thing for `\\g{N}`). By
  giving `\0`, one can inject the whole matched pattern in the replacement
  string.

  When the pattern is a string, a developer can use the replaced part inside
  the `replacement` by using the `:insert_replaced` option and specifying the
  position(s) inside the `replacement` where the string pattern will be
  inserted:

      iex> String.replace("a,b,c", "b", "[]", insert_replaced: 1)
      "a,[b],c"

      iex> String.replace("a,b,c", ",", "[]", insert_replaced: 2)
      "a[],b[],c"

      iex> String.replace("a,b,c", ",", "[]", insert_replaced: [1, 1])
      "a[,,]b[,,]c"

  A compiled pattern can also be given:

      iex> pattern = :binary.compile_pattern(",")
      iex> String.replace("a,b,c", pattern, "[]", insert_replaced: 2)
      "a[],b[],c"

  When an empty string is provided as a `pattern`, the function will treat it as
  an implicit empty string between each grapheme and the string will be
  interspersed. If an empty string is provided as `replacement` the `subject`
  will be returned:

      iex> String.replace("ELIXIR", "", ".")
      ".E.L.I.X.I.R."

      iex> String.replace("ELIXIR", "", "")
      "ELIXIR"

  """
  @spec replace(t, pattern | Regex.t(), t, keyword) :: t
  def replace(subject, pattern, replacement, options \\ [])
  def replace(subject, "", "", _), do: subject

  def replace(subject, "", replacement, options) do
    if Keyword.get(options, :global, true) do
      IO.iodata_to_binary([replacement | intersperse(subject, replacement)])
    else
      replacement <> subject
    end
  end

  def replace(subject, pattern, replacement, options) when is_binary(replacement) do
    if Regex.regex?(pattern) do
      Regex.replace(pattern, subject, replacement, global: options[:global])
    else
      opts = translate_replace_options(options)
      :binary.replace(subject, pattern, replacement, opts)
    end
  end

  defp intersperse(subject, replacement) do
    case next_grapheme(subject) do
      {current, rest} -> [current, replacement | intersperse(rest, replacement)]
      nil -> []
    end
  end

  defp translate_replace_options(options) do
    global = if Keyword.get(options, :global) != false, do: [:global], else: []

    insert =
      if insert = Keyword.get(options, :insert_replaced),
        do: [{:insert_replaced, insert}],
        else: []

    global ++ insert
  end

  @doc ~S"""
  Reverses the graphemes in given string.

  ## Examples

      iex> String.reverse("abcd")
      "dcba"

      iex> String.reverse("hello world")
      "dlrow olleh"

      iex> String.reverse("hello ∂og")
      "go∂ olleh"

  Keep in mind reversing the same string twice does
  not necessarily yield the original string:

      iex> "̀e"
      "̀e"
      iex> String.reverse("̀e")
      "è"
      iex> String.reverse(String.reverse("̀e"))
      "è"

  In the first example the accent is before the vowel, so
  it is considered two graphemes. However, when you reverse
  it once, you have the vowel followed by the accent, which
  becomes one grapheme. Reversing it again will keep it as
  one single grapheme.
  """
  @spec reverse(t) :: t
  def reverse(string) do
    do_reverse(next_grapheme(string), [])
  end

  defp do_reverse({grapheme, rest}, acc) do
    do_reverse(next_grapheme(rest), [grapheme | acc])
  end

  defp do_reverse(nil, acc), do: IO.iodata_to_binary(acc)

  @compile {:inline, duplicate: 2}

  @doc """
  Returns a string `subject` duplicated `n` times.

  Inlined by the compiler.

  ## Examples

      iex> String.duplicate("abc", 0)
      ""

      iex> String.duplicate("abc", 1)
      "abc"

      iex> String.duplicate("abc", 2)
      "abcabc"

  """
  @spec duplicate(t, non_neg_integer) :: t
  def duplicate(subject, n) do
    :binary.copy(subject, n)
  end

  @doc """
  Returns all codepoints in the string.

  For details about codepoints and graphemes, see the `String` module documentation.

  ## Examples

      iex> String.codepoints("olá")
      ["o", "l", "á"]

      iex> String.codepoints("оптими зации")
      ["о", "п", "т", "и", "м", "и", " ", "з", "а", "ц", "и", "и"]

      iex> String.codepoints("ἅἪῼ")
      ["ἅ", "Ἢ", "ῼ"]

      iex> String.codepoints("\u00e9")
      ["é"]

      iex> String.codepoints("\u0065\u0301")
      ["e", "́"]

  """
  @spec codepoints(t) :: [codepoint]
  defdelegate codepoints(string), to: String.Unicode

  @doc """
  Returns the next codepoint in a string.

  The result is a tuple with the codepoint and the
  remainder of the string or `nil` in case
  the string reached its end.

  As with other functions in the String module, this
  function does not check for the validity of the codepoint.
  That said, if an invalid codepoint is found, it will
  be returned by this function.

  ## Examples

      iex> String.next_codepoint("olá")
      {"o", "lá"}

  """
  @compile {:inline, next_codepoint: 1}
  @spec next_codepoint(t) :: {codepoint, t} | nil
  defdelegate next_codepoint(string), to: String.Unicode

  @doc ~S"""
  Checks whether `string` contains only valid characters.

  ## Examples

      iex> String.valid?("a")
      true

      iex> String.valid?("ø")
      true

      iex> String.valid?(<<0xFFFF::16>>)
      false

      iex> String.valid?(<<0xEF, 0xB7, 0x90>>)
      true

      iex> String.valid?("asd" <> <<0xFFFF::16>>)
      false

  """
  @spec valid?(t) :: boolean
  def valid?(string)

  def valid?(<<_::utf8, t::binary>>), do: valid?(t)
  def valid?(<<>>), do: true
  def valid?(_), do: false

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
      {size, rest} -> {binary_part(binary, 0, size), rest}
      nil -> nil
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
