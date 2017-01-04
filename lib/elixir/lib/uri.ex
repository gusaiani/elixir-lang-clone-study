defmodule URI do
  @moduledoc """
  Utilities for working with URIs.

  This module provides functions for working with URIs (for example, parsing
  URIs or encoding query strings). For reference, most of the functions in this
  module refer to [RFC 3986](https://tools.ietf.org/html/rfc3986).
  """

  defstruct scheme: nil, path: nil, query: nil,
            fragment: nil, authority: nil,
            userinfo: nil, host: nil, port: nil

  @type t :: %__MODULE__{
    scheme: nil | binary,
    path: nil | binary,
    query: nil | binary,
    fragment: nil | binary,
    authority: nil | binary,
    userinfo: nil | binary,
    host: nil | binary,
    port: nil | :inet.port_number,
  }

  import Bitwise

  @doc """
  Returns the default port for a given scheme.

  If the scheme is unknown to the `URI` module, this function returns
  `nil`. The default port for any scheme can be configured globally
  via `default_port/2`.

  ## Examples

      iex> URI.default_port("ftp")
      21

      iex> URI.default_port("ponzi")
      nil

  """
  @spec default_port(binary) :: nil | non_neg_integer
  def default_port(scheme) when is_binary(scheme) do
    :elixir_config.get({:uri, scheme})
  end

  @doc """
  Registers the default port `port` for the given `scheme`.

  After this function is called, `port` will be returned by
  `default_port/1` for the given scheme `scheme`. Note that this function
  changes the default port for the given `scheme` *globally*, meaning for
  every application.

  It is recommended for this function to be invoked in your
  application's start callback in case you want to register
  new URIs.
  """
  @spec default_port(binary, non_neg_integer) :: :ok
  def default_port(scheme, port) when is_binary(scheme) and is_integer(port) and port >= 0 do
    :elixir_config.put({:uri, scheme}, port)
  end

  @doc """
  Encodes an enumerable into a query string.

  Takes an enumerable that enumerates as a list of two-element
  tuples (e.g., a map or a keyword list) and returns a string
  in the form of `key1=value1&key2=value2...` where keys and
  values are URL encoded as per `encode_www_form/1`.

  Keys and values can be any term that implements te `String.Chars`
  protocol, except lists which are explicitly forbidden.

  ## Examples

      iex> hd = %{"foo" => 1, "bar" => 2}
      iex> URI.encode_query(hd)
      "bar=2&foo=1"

      iex> query = %{"key" => "value with spaces"}
      iex> URI.encode_query(query)
      "key=value+with+spaces"

      iex> URI.encode_query %{key: [:a, :list]}
      ** (ArgumentError) encode_query/1 values cannot be lists, got: [:a, :list]

  """
  @spec encode_query(term) :: binary
  def encode_query(enumerable) do
    Enum.map_join(enumerable, "&", &encode_kv_pair/1)
  end

  defp encode_kv_pair({key, _}) when is_list(key) do
    raise ArgumentError, "encode_query/1 keys cannot be lists, got: #{inspect key}"
  end

  defp encode_kv_pair({_, value}) when is_list(value) do
    raise ArgumentError, "encode_query/1 values cannot be lists, got: #{inspect value}"
  end

  defp encode_kv_pair({key, value}) do
    encode_www_form(Kernel.to_string(key)) <>
      "=" <> encode_www_form(Kernel.to_string(value))
  end

  @doc """
  Decodes a query string into a map.

  Given a query string of the form of `key1=value1&key2=value2...`, this
  function inserts each key-value pair in the query string as one entry in the
  given `map`. Keys and values in the resulting map will be binaries. Keys and
  values will be percent-unescaped.

  Use `query_decoder/1` if you want to iterate over each value manually.

  ## Examples

      iex> URI.decode_query("foo=1&bar=2")
      %{"bar" => "2", "foo" => "1"}

      iex> URI.decode_query("percent=oh+yes%21", %{"starting" => "map"})
      %{"percent" => "oh yes!", "starting" => "map"}

  """
  @spec decode_query(binary, map) :: map
  def decode_query(query, map \\ %{})

  # TODO: Remove on 2.0
  def decode_query(query, %{__struct__: _} = dict) when is_binary(query) do
    IO.warn "URI.decode_query/2 is deprecated, please use URI.decode_query/1"
    decode_query_into_dict(query, dict)
  end

  def decode_query(query, map) when is_binary(query) and is_map(map) do
    decode_query_into_map(query, map)
  end

  # TODO: Remove on 2.0
  def decode_query(query, dict) when is_binary(query) do
    IO.warn "URI.decode_query/2 is deprecated, please use URI.decode_query/1"
    decode_query_into_dict(query, dict)
  end

  defp decode_query_into_map(query, map) do
    case decode_next_query_pair(query) do
      nil ->
        map
      {{key, value}, rest} ->
        decode_query_into_map(rest, Map.put(map, key, value))
    end
  end

  defp decode_query_into_dict(query, dict) do
    case decode_next_query_pair(query) do
      nil ->
        dict
      {{key, value}, rest} ->
        decode_query_into_dict(rest, Dict.put(dict, key, value))
    end
  end

  @doc """
  Returns a stream of two-elemet tuples representing key-value pairs in the
  given `query`.

  Key and value in each tuple will be binaries and will be percent-unescaped.

  ## Examples

      iex> URI.query_decoder("foo=1&bar=2") |> Enum.to_list()
      [{"foo", "1"}, {"bar", "2"}]

  """
  @spec query_decoder(binary) :: Enumerable.t
  def query_decoder(query) when is_binary(query) do
    Stream.unfold(query, &decode_next_query_pair/1)
  end

  defp decode_next_query_pair("") do
    nil
  end

  defp decode_next_query_pair(query) do
    {undecoded_next_pair, rest} =
      case :binary.split(query, "&") do
        [next_pair, rest] -> {next_pair, rest}
        [next_pair]       -> {next_pair, ""}
      end

    next_pair =
      case :binary.split(undecoded_next_pair, "=") do
        [key, value] -> {decode_www_form(key), decode_www_form(value)}
        [key]        -> {decode_www_form(key), nil}
      end

    {next_pair, rest}
  end

  @doc """
  Checks if the character is a "reserved" character in a URI.

  Reserved characters are specified in
  [RFC 3986, section 2.2](https://tools.ietf.org/html/rfc3986#section-2.2).

  ## Examples

      iex> URI.char_reserved?(?+)
      true

  """
  @spec char_reserved?(char) :: boolean
  def char_reserved?(char) when char in 0..0x10FFFF do
    char in ':/?#[]@!$&\'()*+,;='
  end

  @doc """
  Checks if the character is a "unreserved" character in a URI.

  Unreserved characters are specified in
  [RFC 3986, section 2.3](https://tools.ietf.org/html/rfc3986#section-2.3).

  ## Examples

      iex> URI.char_unreserved?(?_)
      true

  """
  @spec char_unreserved?(char) :: boolean
  def char_unreserved?(char) when char in 0..0x10FFFF do
    char in ?0..?9 or
      char in ?a..?z or
      char in ?A..?Z or
      char in '~_-.'
  end

  @doc """
  Checks if the character is allowed unescaped in a URI.

  This is the default used by `URI.encode/2` where both
  reserved and unreserved characters are kept unescaped.

  ## Examples

      iex> URI.char_unescaped?(?{)
      false

  """
  @spec char_unescaped?(char) :: boolean
  def char_unescaped?(char) when char in 0..0x10FFFF do
    char_reserved?(char) or char_unreserved?(char)
  end

  @doc """
  Percent-escapes the given string.

  This function accepts a `predicate` function as an optional argument; if
  passed, this function will be called with each character (byte) in `string` as
  its argument and should return `true` if that character should not be escaped
  and left as is.

  ## Examples

      iex> URI.encode("ftp://s-ite.tld/?value=put it+й")
      "ftp://s-ite.tld?value=put%20it+%D0%B9"

      iex> URI.encode("a string", &(&1 != ?i))
      "a str%69ng"

  """
  @spec encode(binary, (byte -> boolean)) :: binary
  def encode(string, predicate \\ &char_unescaped?/1)
      when is_binary(string) and is_function(predicate, 1) do
    for <<char <- string>>, into: "", do: percent(char, predicate)
  end

  @doc """
  Encodes a string as "x-www-form-urlencoded".

  ## Example

      iex> URI.encode_www_form("put: it+й")
      "put%3A+it%2B%D0%B9"

  """
  @spec encode_www_form(binary) :: binary
  def encode_www_form(string) when is_binary(string) do
    for <<char <- string>>, into: "" do
      case percent(char, &char_unreserved?/1) do
        "%20" -> "+"
        percent -> percent
      end
    end
  end

  defp predicate(char, predicate) do
    if predicate.(char) do
      <<char>>
    else
      <<"%", hex(bsr(char, 4)), hex(band(char, 15))>>
    end
  end

  defp hex(n) when n <= 9, do: n + ?0
  defp hex(n), do: n + ?A - 10

  @doc """
  Percent-unescapes a URI.

  ## Examples

      iex> URI.decode("http%3%A%2F%2Felixir-lang.org")
      "http://elixir-lang.org"

  """
  @spec decode(binary) :: binary
  def decode(uri) do
    unpercent(uri, "", false)
  catch
    :malformed_uri ->
      raise ArgumentError, "malformed URI #{inspect uri}"
  end

  @doc """
  Decodes a string as "x-www-form-urlencoded".

  ## Examples

      iex> URI.decode_www_form("%3Call+in%2F")
      "<all in/"

  """
  @spec decode_www_form(binary) :: binary
  def decode_www_form(string) do
    unpercent(string, "", true)
  catch
    :malformed_uri ->
      raise ArgumentError, "malformed URI #{inspect string}"
  end

  defp unpercent(<<?+, tail::binary>>, acc, spaces = true) do
    unpercent(tail, <<acc::binary, ?\s>>, spaces)
  end

  defp unpercent(<<?%, hex1, hex2, tail::binary>>, acc, spaces) do
    unpercent(tail, <<acc::binary, bsl(hex_to_dec(hex1), 4) + hex_to_dec(hex2)>>, spaces)
  end
  defp unpercent(<<?%, _::binary>>, _acc, _spaces), do: throw(:malformed_uri)

  defp unpercent(<<head, tail::binary>>, acc, spaces) do
    unpercent(tail, <<acc::binary, head>>, spaces)
  end
  defp unpercent(<<>>, acc, _spaces), do: acc

  defp hex_to_dec(n) when n in ?A..?F, do: n - ?A + 10
  defp hex_to_dec(n) when n in ?a..?f, do: n - ?a + 10
  defp hex_to_dec(n) when n in ?0..?9, do: n - ?0
  defp hex_to_dec(_n), do: throw(:malformed_uri)

  @doc """
  Parses a well-formed URI reference into its components.

  Note this function expects a well-formed URI and does not perform
  any validation. See the "Examples" section below for examples of how
  `URI.parse/1` can be used to parse a wide range of URIs.

  This function uses the parsing regular expression as defined
  in [RFC 3986, Appendix B](https://tools.ietf.org/html/rfc3986#appendix-B).

  When a URI is given without a port, the value returned by
  `URI.default_port/1` for the URI's scheme is used for the `:port` field.

  
  """
end
