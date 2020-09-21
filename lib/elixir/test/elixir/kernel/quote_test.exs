Code.require_file("../test_helper.exs", __DIR__)

defmodule Kernel.QuoteTest do
  use ExUnit.Case, async: true

  @some_fun &List.flatten/1

  test "fun" do
    assert is_function(@some_fun)
  end

  test "list" do
    assert quote(do: [1, 2, 3]) == [1, 2, 3]
  end

  test "tuple" do
    assert quote(do: {:a, 1}) == {:a, 1}
  end

  test "keep line" do
    line = __ENV__.line + 2

    assert quote(location: :keep, do: bar(1, 2, 3)) ==
             {:bar, [keep: {Path.relative_to_cwd(__ENV__.file), line}], [1, 2, 3]}
  end

  test "fixed line" do
    assert quote(line: 3, do: bar(1, 2, 3)) == {:bar, [line: 3], [1, 2, 3]}
    assert quote(line: false, do: bar(1, 2, 3)) == {:bar, [], [1, 2, 3]}
    assert quote(line: true, do: bar(1, 2, 3)) == {:bar, [line: __ENV__.line], [1, 2, 3]}
  end

  test "quote line var" do
    line = __ENV__.line
    assert quote(line: line, do: bar(1, 2, 3)) == {:bar, [line: line], [1, 2, 3]}

    assert_raise ArgumentError, fn ->
      line = "oops"
      quote(line: line, do: bar(1, 2, 3))
    end

    assert_raise ArgumentError, fn ->
      line = true
      quote(line: line, do: bar(1, 2, 3))
    end
  end

  test "quote context var" do
    context = :dynamic
    assert quote(context: context, do: bar) == {:bar, [], :dynamic}

    assert_raise ArgumentError, fn ->
      context = "oops"
      quote(context: context, do: bar)
    end

    assert_raise ArgumentError, fn ->
      context = nil
      quote(context: context, do: bar)
    end
  end

  test "operator precedence" do
    assert {:+, _, [{:+, _, [1, _]}, 1]} = quote(do: 1 + Foo.l() + 1)
  end
end
