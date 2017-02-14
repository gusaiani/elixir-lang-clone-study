defmodule Base do
  import Bitwise

  @moduledoc """
  This module provides data encoding and decoding functions
  according to [RFC 4648](https://tools.ietf.org/html/rfc4648).

  This document defines the commonly used base 16, base 32, and base
  64 encoding schemes.

  ## Base 16 alphabet

      | Value | Encoding | Value | Encoding | Value | Encoding | Value | Encoding |
      |------:|---------:|------:|---------:|------:|---------:|------:|---------:|
      |      0|         0|      4|         4|      8|         8|     12|         C|
      |      1|         1|      5|         5|      9|         9|     13|         D|
      |      2|         2|      6|         6|     10|         A|     14|         E|
      |      3|         3|      7|         7|     11|         B|     15|         F|

  ## Base 32 alphabet

      | Value | Encoding | Value | Encoding | Value | Encoding | Value | Encoding |
      |------:|---------:|------:|---------:|------:|---------:|------:|---------:|
      |      0|         A|      9|         J|     18|         S|     27|         3|
      |      1|         B|     10|         K|     19|         T|     28|         4|
      |      2|         C|     11|         L|     20|         U|     29|         5|
      |      3|         D|     12|         M|     21|         V|     30|         6|
      |      4|         E|     13|         N|     22|         W|     31|         7|
      |      5|         F|     14|         O|     23|         X|       |          |
      |      6|         G|     15|         P|     24|         Y|  (pad)|         =|
      |      7|         H|     16|         Q|     25|         Z|       |          |
      |      8|         I|     17|         R|     26|         2|       |          |


  ## Base 32 (extended hex) alphabet

      | Value | Encoding | Value | Encoding | Value | Encoding | Value | Encoding |
      |------:|---------:|------:|---------:|------:|---------:|------:|---------:|
      |      0|         0|      9|         9|     18|         I|     27|         R|
      |      1|         1|     10|         A|     19|         J|     28|         S|
      |      2|         2|     11|         B|     20|         K|     29|         T|
      |      3|         3|     12|         C|     21|         L|     30|         U|
      |      4|         4|     13|         D|     22|         M|     31|         V|
      |      5|         5|     14|         E|     23|         N|       |          |
      |      6|         6|     15|         F|     24|         O|  (pad)|         =|
      |      7|         7|     16|         G|     25|         P|       |          |
      |      8|         8|     17|         H|     26|         Q|       |          |

  ## Base 64 alphabet

      | Value | Encoding | Value | Encoding | Value | Encoding | Value | Encoding |
      |------:|---------:|------:|---------:|------:|---------:|------:|---------:|
      |      0|         A|     17|         R|     34|         i|     51|         z|
      |      1|         B|     18|         S|     35|         j|     52|         0|
      |      2|         C|     19|         T|     36|         k|     53|         1|
      |      3|         D|     20|         U|     37|         l|     54|         2|
      |      4|         E|     21|         V|     38|         m|     55|         3|
      |      5|         F|     22|         W|     39|         n|     56|         4|
      |      6|         G|     23|         X|     40|         o|     57|         5|
      |      7|         H|     24|         Y|     41|         p|     58|         6|
      |      8|         I|     25|         Z|     42|         q|     59|         7|
      |      9|         J|     26|         a|     43|         r|     60|         8|
      |     10|         K|     27|         b|     44|         s|     61|         9|
      |     11|         L|     28|         c|     45|         t|     62|         +|
      |     12|         M|     29|         d|     46|         u|     63|         /|
      |     13|         N|     30|         e|     47|         v|       |          |
      |     14|         O|     31|         f|     48|         w|  (pad)|         =|
      |     15|         P|     32|         g|     49|         x|       |          |
      |     16|         Q|     33|         h|     50|         y|       |          |

  ## Base 64 (URL and filename safe) alphabet

      | Value | Encoding | Value | Encoding | Value | Encoding | Value | Encoding |
      |------:|---------:|------:|---------:|------:|---------:|------:|---------:|
      |      0|         A|     17|         R|     34|         i|     51|         z|
      |      1|         B|     18|         S|     35|         j|     52|         0|
      |      2|         C|     19|         T|     36|         k|     53|         1|
      |      3|         D|     20|         U|     37|         l|     54|         2|
      |      4|         E|     21|         V|     38|         m|     55|         3|
      |      5|         F|     22|         W|     39|         n|     56|         4|
      |      6|         G|     23|         X|     40|         o|     57|         5|
      |      7|         H|     24|         Y|     41|         p|     58|         6|
      |      8|         I|     25|         Z|     42|         q|     59|         7|
      |      9|         J|     26|         a|     43|         r|     60|         8|
      |     10|         K|     27|         b|     44|         s|     61|         9|
      |     11|         L|     28|         c|     45|         t|     62|         -|
      |     12|         M|     29|         d|     46|         u|     63|         _|
      |     13|         N|     30|         e|     47|         v|       |          |
      |     14|         O|     31|         f|     48|         w|  (pad)|         =|
      |     15|         P|     32|         g|     49|         x|       |          |
      |     16|         Q|     33|         h|     50|         y|       |          |

  """
  b16_alphabet    = Enum.with_index '0123456789ABCDEF'
  b64_alphabet    = Enum.with_index 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/'
  b64url_alphabet = Enum.with_index 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_'
  b32_alphabet    = Enum.with_index 'ABCDEFGHIJKLMNOPQRSTUVWXYZ234567'
  b32hex_alphabet = Enum.with_index '0123456789ABCDEFGHIJKLMNOPQRSTUV'

  Enum.each [{:enc16,    :dec16,    b16_alphabet},
             {:enc32,    :dec32,    b32_alphabet},
             {:enc64,    :dec64,    b64_alphabet},
             {:enc64url, :dec64url, b64url_alphabet},
             {:enc32hex, :dec32hex, b32hex_alphabet}], fn({enc, dec, alphabet}) ->
    for {encoding, value} <- alphabet do
      defp unquote(enc)(unquote(value)), do: unquote(encoding)
      defp unquote(dec)(unquote(encoding)), do: unquote(value)
    end
    defp unquote(dec)(c) do
      raise ArgumentError, "non-alphabet digit found: #{inspect <<c>>, binaries: :as_strings} (byte #{c})"
    end
  end

  @compile {:inline, from_upper: 1, from_lower: 1, from_mixed: 1,
                     to_lower: 1, to_upper: 1, enc16: 1, dec16: 1,
                     enc32: 1, dec32: 1, enc32hex: 1, dec32hex: 1,
                     enc64: 1, dec64: 1, enc64url: 1, dec64url: 1}

  defp to_lower(char) when char in ?A..?Z,
    do: char + (?a - ?A)
  defp to_lower(char),
    do: char

  defp to_upper(char), do: char

  defp from_upper(char), do: char

  defp from_lower(char) when char in ?a..?z,
    do: char - (?a - ?A)
  defp from_lower(char) when char not in ?A..?Z,
    do: char
  defp from_lower(char),
    do: raise(ArgumentError, "non-alphabet digit found: \"#{<<char>>}\" (byte #{char})")

  defp from_mixed(char) when char in ?a..?z,
    do: char - (?a - ?A)
  defp from_mixed(char),
    do: char

  defp maybe_pad(subject, false, _, _),
    do: subject
  defp maybe_pad(subject, _, group_size, pad) do
    case rem(byte_size(subject), group_size) do
      0 -> subject
      x -> subject <> String.duplicate(pad, group_size - x)
    end
  end
end
