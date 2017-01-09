Code.require_file "test_helper.exs", __DIR__

defmodule URITest do
  use ExUnit.Case, async: true

  doctest URI

  test "encode/1,2" do
    assert URI.encode("4_test.is-s~") == "4_test.is-s~"
    assert URI.encode("\r\n&<%>\" ゆ", &URI.char_unreserved?/1) ==
           "%0D%0A%26%3C%25%3E%22%20%E3%82%86"
  end

  test "encode_www_form/1" do
    assert URI.encode_www_form("4test ~1.x") == "4test+~1.x"
    assert URI.encode_www_form("poll:146%") == "poll%3A146%25"
    assert URI.encode_www_form("/\n+/ゆ") == "%2F%0A%2B%2F%E3%82%86"
  end

  test "encode_query/1" do
    assert URI.encode_query([{:foo, :bar}, {:baz, :quux}]) == "foo=bar&baz=quux"
    assert URI.encode_query([{"foo", "bar"}, {"baz", "quux"}]) == "foo=bar&baz=quux"
    assert URI.encode_query([{"foo z", :bar}]) == "foo+z=bar"

    assert_raise ArgumentError, fn ->
      URI.encode_query([{"foo", 'bar'}])
    end

    assert_raise ArgumentError, fn ->
      URI.encode_query([{'foo', "bar"}])
    end
  end

  test "decode_query/1,2" do
    assert URI.decode_query("", %{}) == %{}

    assert URI.decode_query("safe=off", %{"cookie" => "foo"}) ==
           %{"safe" => "off", "cookie" => "foo"}

    assert URI.decode_query("q=search%20query&cookie=ab%26cd&block+buster=") ==
           %{"block buster" => "", "cookie" => "ab&cd", "q" => "search query"}

    assert URI.decode_query("something=weird%3Dhappening") ==
           %{"something" => "weird=happening"}

    assert URI.decode_query("garbage") ==
           %{"garbage" => nil}
    assert URI.decode_query("=value") ==
           %{"" => "value"}
    assert URI.decode_query("something=weird=happening") ==
           %{"something" => "weird=happening"}
  end

  test "query_decoder/1" do
    decoder  = URI.query_decoder("q=search%20query&cookie=ab%26cd&block%20buster=")
    expected = [{"q", "search query"}, {"cookie", "ab&cd"}, {"block buster", ""}]
    assert Enum.map(decoder, &(&1)) == expected
  end

  test "decode/1" do
    assert URI.decode("%0D%0A%26%3C%25%3E%22%20%E3%82%86") == "\r\n&<%>\" ゆ"
    assert URI.decode("%2f%41%4a%55") == "/AJU"
    assert URI.decode("4_t+st.is-s~") == "4_t+st.is-s~"

    assert_raise ArgumentError, ~R/malformed URI/, fn ->
      URI.decode("% invalid")
    end
    assert_raise ArgumentError, ~R/malformed URI/, fn ->
      URI.decode("invalid%")
    end
  end

  test "decode_www_form/1" do
    assert URI.decode_www_form("%3Eval+ue%2B") == ">val ue+"
    assert URI.decode_www_form("%E3%82%86+") == "ゆ "

    assert_raise ArgumentError, fn ->
      URI.decode_www_form("%ZZ")
    end
  end

  describe "parse/1" do
    test "returns the given URI if a %URI{} struct is given" do
      assert URI.parse(uri = %URI{scheme: "http", host: "foo.com"}) == uri
    end

    test "works with HTTP scheme" do
      assert %URI{scheme: "http", host: "foo.com", path: "/path/to/something",
                  query: "foo=bar&bar=foo", fragment: "fragment", port: 80,
                  authority: "foo.com", userinfo: nil} ==
             URI.parse("http://foo.com/path/to/something?foo=bar&bar=foo#fragment")
    end
  end
end
