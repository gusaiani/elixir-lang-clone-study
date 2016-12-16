Code.require_file "test_helper.exs", __DIR__

defmodule Inspect.AtomTest do
  use ExUnit.Case, async: true

  doctest Inspect

  test "basic" do
    assert inspect(:foo) == ":foo"
  end

  test "empty" do
    assert inspect(:"") == ":\"\""
  end

  test "true false nil" do
    assert inspect(false) == "false"
    assert inspect(true) == "true"
    assert inspect(nil) == "nil"
  end

  test "with uppercase" do
    assert inspect(:fOO) == ":fOO"
    assert inspect(:FOO) == ":FOO"
  end

  test "alias atom" do
    assert inspect(Foo) == "Foo"
    assert inspect(Foo.Bar) == "Foo.Bar"
    assert inspect(Elixir) == "Elixir"
    assert inspect(Elixir.Elixir) == "Elixir.Elixir"
  end

  test "with integers" do
    assert inspect(User1)  == "User1"
    assert inspect(:user1) == ":user1"
  end

  test "with punctuation" do
    assert inspect(:foo?) == ":foo?"
    assert inspect(:bar!) == ":bar!"
  end

  test "op" do
    assert inspect(:+) == ":+"
    assert inspect(:<~) == ":<~"
    assert inspect(:~>) == ":~>"
    assert inspect(:&&&) == ":&&&"
    assert inspect(:~~~) == ":~~~"
    assert inspect(:<<~) == ":<<~"
    assert inspect(:~>>) == ":~>>"
    assert inspect(:<~>) == ":<~>"
    assert inspect(:<|>) == ":<|>"
  end

  test :... do
    assert inspect(:...) == ":..."
  end

  test :@ do
    assert inspect(:@) == ":@"
    assert inspect(:foo@bar) == ":foo@bar"
    assert inspect(:foo@bar@) == ":foo@bar@"
    assert inspect(:foo@bar@baz) == ":foo@bar@baz"
  end

  test "others" do
    assert inspect(:<<>>) == ":<<>>"
    assert inspect(:{})   == ":{}"
    assert inspect(:%{})  == ":%{}"
    assert inspect(:%)    == ":%"
  end

  test "colors" do
    opts = [syntax_colors: [atom: :red]]
    assert inspect(:hello, opts) == "\e[31m:hello\e[0m"
    opts = [syntax_colors: [reset: :cyan]]
    assert inspect(:hello, opts) == ":hello"
  end
end

defmodule Inspect.BitStringTest do
  use ExUnit.Case, async: true

  test "bitstring" do
    assert inspect(<<1::12-integer-signed>>) == "<<0, 1::size(4)>>"
  end

  test "binary" do
    assert inspect("foo") == "\"foo\""
    assert inspect(<<?a, ?b, ?c>>) == "\"abc\""
  end

  test "escape" do
    assert inspect("f\no") == "\"f\\no\""
    assert inspect("f\\o") == "\"f\\\\o\""
    assert inspect("f\ao") == "\"f\\ao\""
  end

  test "UTF-8" do
    assert inspect(" ゆんゆん") == "\" ゆんゆん\""
  end

  test "all escapes" do
    assert inspect("\a\b\d\e\f\n\r\s\t\v") ==
           "\"\\a\\b\\d\\e\\f\\n\\r \\t\\v\""
  end

  test "opt infer" do
    assert inspect(<<"john", 193, "doe">>, binaries: :infer) == ~s(<<106, 111, 104, 110, 193, 100, 111, 101>>)
    assert inspect(<<"john">>, binaries: :infer) == ~s("john")
    assert inspect(<<193>>, binaries: :infer) == ~s(<<193>>)
  end

  test "opt as strings" do
    assert inspect(<<"john", 193, "doe">>, binaries: :as_strings) == ~s("john\\xC1doe")
    assert inspect(<<"john">>, binaries: :as_strings) == ~s("john")
    assert inspect(<<193>>, binaries: :as_strings) == ~s("\\xC1")
  end

  test "opt as binaries" do
    assert inspect(<<"john", 193, "doe">>, binaries: :as_binaries) == "<<106, 111, 104, 110, 193, 100, 111, 101>>"
    assert inspect(<<"john">>, binaries: :as_binaries) == "<<106, 111, 104, 110>>"
    assert inspect(<<193>>, binaries: :as_binaries) == "<<193>>"
    # base: :hex is recognized
    assert inspect("abc", binaries: :as_binary, base: :hex) == "<<0x61, 0x62, 0x63>>"
    # any base other than :decimal implies binaries: :as_binaries
    assert inspect("abc", base: :hex) == "<<0x61, 0x62, 0x63>>"
    assert inspect("abc", base: :octal) == "<<0o141, 0o142, 0o143>>"
    # size is still represented as decimal
    assert inspect(<<10, 11, 12::4>>, base: :hex) == "<<0xA, 0xB, 0xC::size(4)>>"
  end

  test "unprintable with opts" do
    assert inspect(<<193, 193, 193, 193>>, limit: 3) == "<<193, 193, 193, ...>>"
  end
end
