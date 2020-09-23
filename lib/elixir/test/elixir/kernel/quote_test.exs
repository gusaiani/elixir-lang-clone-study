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
    assert {:+, _, [1, {_, _, [{:+, _, [1]}]}]} = quote(do: 1 + Foo.l(+1))
  end

  test "generated" do
    assert quote(generated: true, do: bar(1)) == {:bar, [generated: true], [1]}
  end

  test "unquote call" do
    assert quote(do: foo(bar)[unquote(:baz)]) == quote(do: foo(bar)[:baz])
    assert quote(do: unquote(:bar)()) == quote(do: bar())

    assert (quote do
              unquote(:bar)(1) do
                2 + 3
              end
            end) ==
             (quote do
                bar 1 do
                  2 + 3
                end
              end)

    assert quote(do: foo.unquote(:bar)) == quote(do: foo.bar)
    assert quote(do: foo.unquote(:bar)()) == quote(do: foo.bar())
    assert quote(do: foo.unquote(:bar)(1)) == quote(do: foo.bar(1))

    assert (quote do
              foo.unquote(:bar)(1) do
                2 + 3
              end
            end) ==
             (quote do
                foo.bar 1 do
                  2 + 3
                end
              end)

    assert quote(do: foo.unquote({:bar, [], nil})) == quote(do: foo.bar)
    assert quote(do: foo.unquote({:bar, [], nil})()) == quote(do: foo.bar())
    assert quote(do: foo.unquote({:bar, [], [1, 2]})) == quote(do: foo.bar(1, 2))

    assert Code.eval_quoted(quote(do: Foo.unquote(Bar))) == {Elixir.Foo.Bar, []}
    assert Code.eval_quoted(quote(do: Foo.unquote(quote(do: Bar)))) == {Elixir.Foo.Bar, []}

    assert_raise ArgumentError, fn ->
      quote(do: foo.unquote(1))
    end
  end

  test "nested quote" do
    assert {:quote, _, [[do: {:unquote, _, _}]]} = quote(do: quote(do: unquote(x)))
  end

  defmacrop nested_quote_in_macro do
    x = 1

    quote do
      x = unquote(x)

      quote do
        unquote(x)
      end
    end
  end

  test "nested quote in macro" do
    assert nested_quote_in_macro() == 1
  end

  defmodule Dyn do
    for {k, v} <- [foo: 1, bar: 2, baz: 3] do
      # Local call unquote
      def unquote(k)(), do: unquote(v)

      # Remote call unquote
      def unquote(k)(arg), do: __MODULE__.unquote(k)() + arg
    end
  end

  test "dynamic definition with unquote" do
    assert Dyn.foo() == 1
    assert Dyn.bar() == 2
    assert Dyn.baz() == 3

    assert Dyn.foo(1) == 2
    assert Dyn.bar(2) == 4
    assert Dyn.baz(3) == 6
  end

  test "splice on root" do
    contents = [1, 2, 3]

    assert quote(do: (unquote_splicing(contents))) ==
             (quote do
                1
                2
                3
              end)
  end

  test "splice with tail" do
    contents = [1, 2, 3]

    assert quote(do: [unquote_splicing(contents) | [1, 2, 3]]) == [1, 2, 3, 1, 2, 3]

    assert quote(do: [unquote_splicing(contents) | val]) == quote(do: [1, 2, 3 | val])

    assert quote(do: [unquote_splicing(contents) | unquote([4])]) == quote(do: [1, 2, 3, 4])
  end

  test "splice on stab" do
    {fun, []} = Code.eval_quoted(quote(do: fn unquote_splicing([1, 2, 3]) -> :ok end), [])
    assert fun.(1, 2, 3) == :ok

    {fun, []} = Code.eval_quoted(quote(do: fn 1, unquote_splicing([2, 3]) -> :ok end), [])
    assert fun.(1, 2, 3) == :ok
  end

  test "splice on definition" do
    defmodule Hello do
      def world([unquote_splicing(["foo", "bar"]) | rest]) do
        rest
      end
    end

    assert Hello.world(["foo", "bar", "baz"]) == ["baz"]
  end

  test "splice on map" do
    assert %{unquote_splicing(foo: :bar)} == %{foo: :bar}
    assert %{unquote_splicing(foo: :bar), baz: :bat} == %{foo: :bar, baz: :bat}
    assert %{unquote_splicing(foo: :bar), :baz => :bat} == %{foo: :bar, baz: :bat}
    assert %{:baz => :bat, unquote_splicing(foo: :bar)} == %{foo: :bar, baz: :bat}

    map = %{foo: :default}
    assert %{map | unquote_splicing(foo: :bar)} == %{foo: :bar}
  end
end
