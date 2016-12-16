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

defmodule Inspect.NumberTest do
  use ExUnit.Case, async: true

  test "integer" do
    assert inspect(100) == "100"
  end

  test "decimal" do
    assert inspect(100, base: :decimal) == "100"
  end

  test "hex" do
    assert inspect(100, base: :hex) == "0x64"
  end

  test "octal" do
    assert inspect(100, base: :octal) == "0o144"
  end

  test "binary" do
    assert inspect(86, base: :binary) == "0b1010110"
  end

  test "float" do
    assert inspect(1.0) == "1.0"
    assert inspect(1.0E10) == "1.0e10"
    assert inspect(1.0e10) == "1.0e10"
    assert inspect(1.0e-10) == "1.0e-10"
  end

  test "integer colors" do
    opts = [syntax_colors: [number: :red]]
    assert inspect(123, opts) == "\e[31m123\e[0m"
    opts = [syntax_colors: [reset: :cyan]]
    assert inspect(123, opts) == "123"
  end

  test "float colors" do
    opts = [syntax_colors: [number: :red]]
    assert inspect(1.3, opts) == "\e[31m1.3\e[0m"
    opts = [syntax_colors: [reset: :cyan]]
    assert inspect(1.3, opts) == "1.3"
  end
end

defmodule Inspect.TupleTest do
  use ExUnit.Case

  test "basic" do
    assert inspect({1, "b", 3}) == "{1, \"b\", 3}"
    assert inspect({1, "b", 3}, [pretty: true, width: 1]) == "{1,\n \"b\",\n 3}"
  end

  test "empty" do
    assert inspect({}) == "{}"
  end

  test "with limit" do
    assert inspect({1, 2, 3, 4}, limit: 3) == "{1, 2, 3, ...}"
  end

  test "colors" do
    opts = [syntax_colors: []]
    assert inspect({}, opts) == "{}"

    opts = [syntax_colors: [reset: :cyan]]
    assert inspect({}, opts) == "{}"
    assert inspect({:x, :y}, opts) == "{:x, :y}"

    opts = [syntax_colors: [reset: :cyan, atom: :red]]
    assert inspect({}, opts) == "{}"
    assert inspect({:x, :y}, opts) ==
      "{\e[31m:x\e[36m, \e[31m:y\e[36m}"

    opts = [syntax_colors: [tuple: :green, reset: :cyan, atom: :red]]
    assert inspect({}, opts) == "\e[32m{\e[36m\e[32m}\e[36m"
    assert inspect({:x, :y}, opts) ==
           "\e[32m{\e[36m" <>
           "\e[31m:x\e[36m" <>
           "\e[32m,\e[36m " <>
           "\e[31m:y\e[36m" <>
           "\e[32m}\e[36m"
  end
end

defmodule Inspect.ListTest do
  use ExUnit.Case, async: true

  test "basic" do
    assert inspect([ 1, "b", 3 ]) == "[1, \"b\", 3]"
    assert inspect([ 1, "b", 3 ], [pretty: true, width: 1]) == "[1,\n \"b\",\n 3]"
  end

  test "printable" do
    assert inspect('abc') == "'abc'"
  end

  test "keyword" do
    assert inspect([a: 1]) == "[a: 1]"
    assert inspect([a: 1, b: 2]) == "[a: 1, b: 2]"
    assert inspect([a: 1, a: 2, b: 2]) == "[a: 1, a: 2, b: 2]"
    assert inspect(["123": 1]) == ~s(["123": 1])

    assert inspect([foo: [1, 2, 3, :bar], bazzz: :bat], [pretty: true, width: 30]) ==
           "[foo: [1, 2, 3, :bar],\n bazzz: :bat]"
  end

  test "opt infer" do
    assert inspect('john' ++ [0] ++ 'doe', charlists: :infer) == "[106, 111, 104, 110, 0, 100, 111, 101]"
    assert inspect('john', charlists: :infer) == "'john'"
    assert inspect([0], charlists: :infer) == "[0]"
  end

  test "opt as strings" do
    assert inspect('john' ++ [0] ++ 'doe', charlists: :as_charlists) == "'john\\0doe'"
    assert inspect('john', charlists: :as_charlists) == "'john'"
    assert inspect([0], charlists: :as_charlists) == "'\\0'"
  end

  test "opt as lists" do
    assert inspect('john' ++ [0] ++ 'doe', charlists: :as_lists) == "[106, 111, 104, 110, 0, 100, 111, 101]"
    assert inspect('john', charlists: :as_lists) == "[106, 111, 104, 110]"
    assert inspect([0], charlists: :as_lists) == "[0]"
  end

  test "non printable" do
    assert inspect([{:b, 1}, {:a, 1}]) == "[b: 1, a: 1]"
  end

  test "improper" do
    assert inspect([:foo | :bar]) == "[:foo | :bar]"

    assert inspect([1, 2, 3, 4, 5 | 42], [pretty: true, width: 1]) == "[1,\n 2,\n 3,\n 4,\n 5 |\n 42]"
  end

  test "nested" do
    assert inspect(Enum.reduce(1..100, [0], &[&2, Integer.to_string(&1)]), [limit: 5]) ==
           "[[[[[[...], ...], \"97\"], \"98\"], \"99\"], \"100\"]"
    assert inspect(Enum.reduce(1..100, [0], &[&2 | Integer.to_string(&1)]), [limit: 5]) ==
           "[[[[[[...] | \"96\"] | \"97\"] | \"98\"] | \"99\"] | \"100\"]"
  end

  test "codepoints" do
    assert inspect('é') == "[233]"
  end

  test "empty" do
    assert inspect([]) == "[]"
  end

  test "with limit" do
    assert inspect([ 1, 2, 3, 4 ], limit: 3) == "[1, 2, 3, ...]"
  end

  test "colors" do
    opts = [syntax_colors: []]
    assert inspect([], opts) == "[]"

    opts = [syntax_colors: [reset: :cyan]]
    assert inspect([], opts) == "[]"
    assert inspect([:x, :y], opts) ==
           "[:x, :y]"

    opts = [syntax_colors: [reset: :cyan, atom: :red]]
    assert inspect([], opts) == "[]"
    assert inspect([:x, :y], opts) ==
           "[\e[31m:x\e[36m, \e[31m:y\e[36m]"

    opts = [syntax_colors: [reset: :cyan, atom: :red, list: :green]]
    assert inspect([], opts) == "\e[32m[]\e[36m"
    assert inspect([:x, :y], opts) ==
           "\e[32m[\e[36m" <>
           "\e[31m:x\e[36m" <>
           "\e[32m,\e[36m " <>
           "\e[31m:y\e[36m" <>
           "\e[32m]\e[36m"
  end

  test "keyword with colors" do
    opts = [syntax_colors: [reset: :cyan, list: :green, number: :blue]]
    assert inspect([], opts) == "\e[32m[]\e[36m"
    assert inspect([a: 9999], opts) ==
           "\e[32m[\e[36m" <>
           "a: " <>
           "\e[34m9999\e[36m" <>
           "\e[32m]\e[36m"

    opts = [syntax_colors: [reset: :cyan, atom: :red, list: :green, number: :blue]]
    assert inspect([], opts) == "\e[32m[]\e[36m"
    assert inspect([a: 9999], opts) ==
           "\e[32m[\e[36m" <>
           "\e[31ma: \e[36m" <>
           "\e[34m9999\e[36m" <>
           "\e[32m]\e[36m"
  end

  test "limit with colors" do
    opts = [limit: 1, syntax_colors: [reset: :cyan, list: :green, atom: :red]]
    assert inspect([], opts) == "\e[32m[]\e[36m"
    assert inspect([:x, :y], opts) ==
           "\e[32m[\e[36m" <>
           "\e[31m:x\e[36m" <>
           "\e[32m,\e[36m " <>
           "..." <>
           "\e[32m]\e[36m"
  end
end
