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
end
