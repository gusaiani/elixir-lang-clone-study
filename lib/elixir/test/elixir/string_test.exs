Code.require_file "test_helper.exs", __DIR__

defmodule StringTest do
  use ExUnit.Case, async: true

  doctest String

  test "next codepoint" do
    assert String.next_codepoint("ésoj") == {"é", "soj"}
    assert String.next_codepoint(<<255>>) == {<<255>>, ""}
    assert String.next_codepoint("") == nil
  end

  # test cases described in http://mortoray.com/2013/11/27/the-string-type-is-broken/
  test "Unicode" do
    assert String.reverse("noël") == "lëon"
    assert String.slice("noël", 0..2) == "noë"
    assert String.length("noël") == 4

    assert String.length("") == 2
    assert String.slice("", 1..1) == ""
    assert String.reverse("") == ""

    assert String.upcase("baffle") == "BAFFLE"

    assert String.equivalent?("noël", "noël")
  end
end
