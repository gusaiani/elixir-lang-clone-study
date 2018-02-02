Code.require_file("test_helper.exs", __DIR__)

defmodule Macro.ExternalTest do
  defmacro external do
    line = 18
    file = __ENV__.file
    ^line = __CALLER__.line
    ^file = __CALLER__.file
    ^line = Macro.Env.location(__CALLER__)[:line]
    ^file = Macro.Env.location(__CALLER__)[:file]
  end

  defmacro oror(left, right) do
    quote(do: unquote(left) || unquote(right))
  end
end

defmodule MacroTest do
  use ExUnit.Case, async: true

  # Changing the lines above will make compilation
  # fail since we are asserting on the caller lines
  import Macro.ExternalTest

  describe "escape/2" do
    test "handles tuples with size different than two" do
      assert Macro.escape({:a}) == {:{}, [], [:a]}
      assert Macro.escape({:a, :b, :c}) == {:{}, [], [:a, :b, :c]}
      assert Macro.escape({:a, {1, 2, 3}, :c}) == {:{}, [], [:a, {:{}, [], [1, 2, 3]}, :c]}
    end

    test "simply returns tuples with size equal to two" do
      assert Macro.escape({:a, :b}) == {:a, :b}
    end

    test "simply returns any other structure" do
      assert Macro.escape([1, 2, 3]) == [1, 2, 3]
    end

    test "handles maps" do
      assert Macro.escape(%{a: 1}) == {:%{}, [], [a: 1]}
    end

    test "handles bitstring" do
      assert {:<<>>, [], args} = Macro.escape(<<300::12>>)
      assert [{:::, [], [1, {:size, [], [4]}]}, {:::, [], [",", {:binary, [], []}]}] = args
    end

    test "works recursively" do
      assert Macro.escape([1, {:a, :b, :c}, 3]) == [1, {:{}, [], [:a, :b, :c]}, 3]
    end

    test "with improper lists" do
      assert Macro.escape([1 | 2]) == [{:|, [], [1, 2]}]
      assert Macro.escape([1, 2 | 3]) == [1, {:|, [], [2, 3]}]
    end

    test "with unquote" do
      contents = quote(unquote: false, do: unquote(1))
      assert Macro.escape(contents, unquote: true) == 1

      contents = quote(unquote: false, do: unquote(x))
      assert Macro.escape(contents, unquote: true) == {:x, [], MacroTest}
    end

    defp eval_escaped(contents) do
      {eval, []} = Code.eval_quoted(Macro.escape(contents, unquote: true))
      eval
    end

    test "with remote unquote" do
      contents = quote(unquote: false, do: Kernel.unquote(:is_atom)(:ok))
      assert eval_escaped(contents) == quote(do: Kernel.is_atom(:ok))
    end

    test "with nested unquote" do
      contents =
        quote do
          quote(do: unquote(x))
        end

      assert eval_escaped(contents) == quote(do: quote(do: unquote(x)))
    end

    test "with alias or no arguments remote unquote" do
      contents = quote(unquote: false, do: Kernel.unquote(:self))
      assert eval_escaped(contents) == quote(do: Kernel.self())

      contents = quote(unquote: false, do: x.unquote(Foo))
      assert eval_escaped(contents) == quote(do: x.unquote(Foo))
    end

    test "with splicing" do
      contents = quote(unquote: false, do: [1, 2, 3, 4, 5])
      assert Macro.escape(contents, unquote: true) == [1, 2, 3, 4, 5]

      contents = quote(unquote: false, do: [1, 2, unquote_splicing([3, 4, 5])])
      assert eval_escaped(contents) == [1, 2, 3, 4, 5]

      contents = quote(unquote: false, do: [unquote_splicing([1, 2, 3]), 4, 5])
      assert eval_escaped(contents) == [1, 2, 3, 4, 5]

      contents =
        quote(unquote: false, do: [unquote_splicing([1, 2, 3]), unquote_splicing([4, 5])])

      assert eval_escaped(contents) == [1, 2, 3, 4, 5]

      contents =
        quote(unquote: false, do: [1, unquote_splicing([2]), 3, unquote_splicing([4]), 5])

      assert eval_escaped(contents) == [1, 2, 3, 4, 5]

      contents =
        quote(unquote: false, do: [1, unquote_splicing([2]), 3, unquote_splicing([4]) | [5]])

      assert eval_escaped(contents) == [1, 2, 3, 4, 5]
    end
  end

  describe "expand_once/2" do
    test "with external macro" do
      assert {:||, _, [1, false]} = Macro.expand_once(quote(do: oror(1, false)), __ENV__)
    end

    test "with raw atom" do
      assert Macro.expand_once(quote(do: :foo), __ENV__) == :foo
    end

    test "with current module" do
      assert Macro.expand_once(quote(do: __MODULE__), __ENV__) == __MODULE__
    end

    test "with main" do
      assert Macro.expand_once(quote(do: Elixir), __ENV__) == Elixir
    end

    test "with simple alias" do
      assert Macro.expand_once(quote(do: Foo), __ENV__) == Foo
    end

    test "with current module plus alias" do
      assert Macro.expand_once(quote(do: __MODULE__.Foo), __ENV__) == __MODULE__.Foo
    end

    test "with main plus alias" do
      assert Macro.expand_once(quote(do: Elixir.Foo), __ENV__) == Foo
    end

    test "with custom alias" do
      alias Foo, as: Bar
      assert Macro.expand_once(quote(do: Bar.Baz), __ENV__) == Foo.Baz
    end

    test "with main plus custom alias" do
      alias Foo, as: Bar, warn: false
      assert Macro.expand_once(quote(do: Elixir.Bar.Baz), __ENV__) == Elixir.Bar.Baz
    end

    test "with call in alias" do
      assert Macro.expand_once(quote(do: Foo.bar().Baz), __ENV__) == quote(do: Foo.bar().Baz)
    end

    test "env" do
      env = %{__ENV__ | line: 0}
      assert Macro.expand_once(quote(do: __ENV__), env) == {:%{}, [], Map.to_list(env)}
      assert Macro.expand_once(quote(do: __ENV__.file), env) == env.file
      assert Macro.expand_once(quote(do: __ENV__.unknown), env) == quote(do: __ENV__.unknown)
    end

    defmacro local_macro() do
      :local_macro
    end

    test "local macro" do
      assert Macro.expand_once(quote(do: local_macro), __ENV__) == :local_macro
    end

    test "checks vars" do
      local_macro = 1
      assert local_macro == 1
      expr = {:local_macro, [], nil}
      assert Macro.expand_once(expr, __ENV__) == expr
    end

    defp expand_once_and_clean(quoted, env) do
      cleaner = &Keyword.drop(&1, [:counter])

      quoted
      |> Macro.expand_once(env)
      |> Macro.prewalk(&Macro.update_meta(&1, cleaner))
    end

    test "with imported macro" do
      temp_var = {:x, [], Kernel}

      quoted =
        quote context: Kernel do
          case 1 do
            unquote(temp_var) when :"Elixir.Kernel".in(unquote(temp_var), [false, nil]) -> false
            unquote(temp_var) -> unquote(temp_var)
          end
        end

      assert expand_once_and_clean(quote(do: 1 || false), __ENV__) == quoted
    end

    test "with require macro" do
      temp_var = {:x, [], Kernel}

      quoted =
        quote context: Kernel do
          case 1 do
            unquote(temp_var) when :"Elixir.Kernel".in(unquote(temp_var), [false, nil]) -> false
            unquote(temp_var) -> unquote(temp_var)
          end
        end

      assert expand_once_and_clean(quote(do: Kernel.||(1, false)), __ENV__) == quoted
    end

    test "with not expandable expression" do
      expr = quote(do: other(1, 2, 3))
      assert Macro.expand_once(expr, __ENV__) == expr
    end

    test "does not expand module attributes" do
      message =
        "could not call get_attribute with argument #{inspect(__MODULE__)} " <>
          "because the module is already compiled"

      assert_raise ArgumentError, message, fn ->
        Macro.expand_once(quote(do: @foo), __ENV__)
      end
    end
  end

  defp expand_and_clean(quoted, env) do
    cleaner = &Keyword.drop(&1, [:counter])

    quoted
    |> Macro.expand(env)
    |> Macro.prewalk(&Macro.update_meta(&1, cleaner))
  end

  test "expand/2" do
    temp_var = {:x, [], Kernel}

    quoted =
      quote context: Kernel do
        case 1 do
          unquote(temp_var) when :"Elixir.Kernel".in(unquote(temp_var), [false, nil]) -> false
          unquote(temp_var) -> unquote(temp_var)
        end
      end

    assert expand_and_clean(quote(do: oror(1, false)), __ENV__) == quoted
  end

  test "var/2" do
    assert Macro.var(:foo, nil) == {:foo, [], nil}
    assert Macro.var(:foo, Other) == {:foo, [], Other}
  end

  describe "to_string/1" do
    test "variable" do
      assert Macro.to_string(quote(do: foo)) == "foo"
    end

    test "local call" do
      assert Macro.to_string(quote(do: foo(1, 2, 3))) == "foo(1, 2, 3)"
      assert Macro.to_string(quote(do: foo([1, 2, 3]))) == "foo([1, 2, 3])"
    end

    test "remote call" do
      assert Macro.to_string(quote(do: foo.bar(1, 2, 3))) == "foo.bar(1, 2, 3)"
      assert Macro.to_string(quote(do: foo.bar([1, 2, 3]))) == "foo.bar([1, 2, 3])"

      quoted =
        quote do
          (foo do
             :ok
           end).bar([1, 2, 3])
        end

      assert Macro.to_string(quoted) == "(foo() do\n  :ok\nend).bar([1, 2, 3])"
    end

    test "atom remote call" do
      assert Macro.to_string(quote(do: :foo.bar(1, 2, 3))) == ":foo.bar(1, 2, 3)"
    end

    test "remote and fun call" do
      assert Macro.to_string(quote(do: foo.bar.(1, 2, 3))) == "foo.bar().(1, 2, 3)"
      assert Macro.to_string(quote(do: foo.bar.([1, 2, 3]))) == "foo.bar().([1, 2, 3])"
    end

    test "unusual remote atom fun call" do
      assert Macro.to_string(quote(do: Foo."42"())) == ~s/Foo."42"()/
      assert Macro.to_string(quote(do: Foo."Bar"())) == ~s/Foo."Bar"()/
      assert Macro.to_string(quote(do: Foo."bar baz"()."")) == ~s/Foo."bar baz"().""()/
      assert Macro.to_string(quote(do: Foo."%{}"())) == ~s/Foo."%{}"()/
      assert Macro.to_string(quote(do: Foo."..."())) == ~s/Foo."..."()/
    end

    test "atom fun call" do
      assert Macro.to_string(quote(do: :foo.(1, 2, 3))) == ":foo.(1, 2, 3)"
    end

    test "aliases call" do
      assert Macro.to_string(quote(do: Foo.Bar.baz(1, 2, 3))) == "Foo.Bar.baz(1, 2, 3)"
      assert Macro.to_string(quote(do: Foo.Bar.baz([1, 2, 3]))) == "Foo.Bar.baz([1, 2, 3])"
      assert Macro.to_string(quote(do: Foo.bar(<<>>, []))) == "Foo.bar(<<>>, [])"
    end

    test "keyword call" do
      assert Macro.to_string(quote(do: Foo.bar(foo: :bar))) == "Foo.bar(foo: :bar)"
      assert Macro.to_string(quote(do: Foo.bar("Elixir.Foo": :bar))) == "Foo.bar([{Foo, :bar}])"
    end

    test "sigil call" do
      assert Macro.to_string(quote(do: ~r"123")) == ~s/~r"123"/
      assert Macro.to_string(quote(do: ~r"123"u)) == ~s/~r"123"u/
      assert Macro.to_string(quote(do: ~r"\n123")) == ~s/~r"\\\\n123"/

      assert Macro.to_string(quote(do: ~r"1#{two}3")) == ~S/~r"1#{two}3"/
      assert Macro.to_string(quote(do: ~r"1#{two}3"u)) == ~S/~r"1#{two}3"u/

      assert Macro.to_string(quote(do: ~R"123")) == ~s/~R"123"/
      assert Macro.to_string(quote(do: ~R"123"u)) == ~s/~R"123"u/
      assert Macro.to_string(quote(do: ~R"\n123")) == ~s/~R"\\\\n123"/
    end

    test "tuple call" do
      assert Macro.to_string(quote(do: alias(Foo.{Bar, Baz, Bong}))) ==
               "alias(Foo.{Bar, Baz, Bong})"

      assert Macro.to_string(quote(do: foo(Foo.{}))) == "foo(Foo.{})"
    end

    test "arrow" do
      assert Macro.to_string(quote(do: foo(1, (2 -> 3)))) == "foo(1, (2 -> 3))"
    end

    test "block" do
      quoted =
        quote do
          1
          2

          (
            :foo
            :bar
          )

          3
        end

      expected = """
      (
        1
        2
        (
          :foo
          :bar
        )
        3
      )
      """

      assert Macro.to_string(quoted) <> "\n" == expected
    end

    test "not in" do
      assert Macro.to_string(quote(do: false not in [])) == "false not in []"
    end

    test "if else" do
      expected = """
      if(foo) do
        bar
      else
        baz
      end
      """

      assert Macro.to_string(quote(do: if(foo, do: bar, else: baz))) <> "\n" == expected
    end

    test "case" do
      quoted =
        quote do
          case foo do
            true ->
              0

            false ->
              1
              2
          end
        end

      expected = """
      case(foo) do
        true ->
          0
        false ->
          1
          2
      end
      """

      assert Macro.to_string(quoted) <> "\n" == expected
    end

    test "try" do
      quoted =
        quote do
          try do
            foo
          catch
            _, _ ->
              2
          rescue
            ArgumentError ->
              1
          after
            4
          else
            _ ->
              3
          end
        end

      expected = """
      try() do
        foo
      rescue
        ArgumentError ->
          1
      catch
        _, _ ->
          2
      else
        _ ->
          3
      after
        4
      end
      """

      assert Macro.to_string(quoted) <> "\n" == expected
    end

    test "fn" do
      assert Macro.to_string(quote(do: fn -> 1 + 2 end)) == "fn -> 1 + 2 end"
      assert Macro.to_string(quote(do: fn x -> x + 1 end)) == "fn x -> x + 1 end"

      quoted =
        quote do
          fn x ->
            y = x + 1
            y
          end
        end

      expected = """
      fn x ->
        y = x + 1
        y
      end
      """

      assert Macro.to_string(quoted) <> "\n" == expected

      quoted =
        quote do
          fn
            x ->
              y = x + 1
              y

            z ->
              z
          end
        end

      expected = """
      fn
        x ->
          y = x + 1
          y
        z ->
          z
      end
      """

      assert Macro.to_string(quoted) <> "\n" == expected

      assert Macro.to_string(quote(do: (fn x -> x end).(1))) == "(fn x -> x end).(1)"

      quoted =
        quote do
          (fn
             %{} -> :map
             _ -> :other
           end).(1)
        end

      expected = """
      (fn
        %{} ->
          :map
        _ ->
          :other
      end).(1)
      """

      assert Macro.to_string(quoted) <> "\n" == expected
    end

    test "range" do
      assert Macro.to_string(quote(do: unquote(-1..+2))) == "-1..2"
      assert Macro.to_string(quote(do: Foo.integer()..3)) == "Foo.integer()..3"
    end

    test "when" do
      assert Macro.to_string(quote(do: (() -> x))) == "(() -> x)"
      assert Macro.to_string(quote(do: (x when y -> z))) == "(x when y -> z)"
      assert Macro.to_string(quote(do: (x, y when z -> w))) == "((x, y) when z -> w)"
      assert Macro.to_string(quote(do: (x, y when z -> w))) == "((x, y) when z -> w)"
    end

    test "nested" do
      quoted =
        quote do
          defmodule Foo do
            def foo do
              1 + 1
            end
          end
        end

      expected = """
      defmodule(Foo) do
        def(foo) do
          1 + 1
        end
      end
      """

      assert Macro.to_string(quoted) <> "\n" == expected
    end

    test "operator precedence" do
      assert Macro.to_string(quote(do: (1 + 2) * (3 - 4))) == "(1 + 2) * (3 - 4)"
      assert Macro.to_string(quote(do: (1 + 2) * 3 - 4)) == "(1 + 2) * 3 - 4"
      assert Macro.to_string(quote(do: 1 + 2 + 3)) == "1 + 2 + 3"
      assert Macro.to_string(quote(do: 1 + 2 - 3)) == "1 + 2 - 3"
    end

    test "capture operator" do
      assert Macro.to_string(quote(do: &foo/0)) == "&foo/0"
      assert Macro.to_string(quote(do: &Foo.foo/0)) == "&Foo.foo/0"
      assert Macro.to_string(quote(do: &(&1 + &2))) == "&(&1 + &2)"
      assert Macro.to_string(quote(do: & &1)) == "&(&1)"
      assert Macro.to_string(quote(do: & &1.(:x))) == "&(&1.(:x))"
      assert Macro.to_string(quote(do: (& &1).(:x))) == "(&(&1)).(:x)"
    end

    test "containers" do
      assert Macro.to_string(quote(do: {})) == "{}"
      assert Macro.to_string(quote(do: [])) == "[]"
      assert Macro.to_string(quote(do: {1, 2, 3})) == "{1, 2, 3}"
      assert Macro.to_string(quote(do: [1, 2, 3])) == "[1, 2, 3]"
      assert Macro.to_string(quote(do: ["Elixir.Foo": :bar])) == "[{Foo, :bar}]"
      assert Macro.to_string(quote(do: %{})) == "%{}"
      assert Macro.to_string(quote(do: %{:foo => :bar})) == "%{foo: :bar}"
      assert Macro.to_string(quote(do: %{:"Elixir.Foo" => :bar})) == "%{Foo => :bar}"
      assert Macro.to_string(quote(do: %{{1, 2} => [1, 2, 3]})) == "%{{1, 2} => [1, 2, 3]}"
      assert Macro.to_string(quote(do: %{map | "a" => "b"})) == "%{map | \"a\" => \"b\"}"
      assert Macro.to_string(quote(do: [1, 2, 3])) == "[1, 2, 3]"
    end

    test "struct" do
      assert Macro.to_string(quote(do: %Test{})) == "%Test{}"
      assert Macro.to_string(quote(do: %Test{foo: 1, bar: 1})) == "%Test{foo: 1, bar: 1}"
      assert Macro.to_string(quote(do: %Test{struct | foo: 2})) == "%Test{struct | foo: 2}"
      assert Macro.to_string(quote(do: %Test{} + 1)) == "%Test{} + 1"
    end

    test "binary operators" do
      assert Macro.to_string(quote(do: 1 + 2)) == "1 + 2"
      assert Macro.to_string(quote(do: [1, 2 | 3])) == "[1, 2 | 3]"
      assert Macro.to_string(quote(do: [h | t] = [1, 2, 3])) == "[h | t] = [1, 2, 3]"
      assert Macro.to_string(quote(do: (x ++ y) ++ z)) == "(x ++ y) ++ z"
    end

    test "unary operators" do
      assert Macro.to_string(quote(do: not 1)) == "not(1)"
      assert Macro.to_string(quote(do: not foo)) == "not(foo)"
      assert Macro.to_string(quote(do: -1)) == "-1"
      assert Macro.to_string(quote(do: +(+1))) == "+(+1)"
      assert Macro.to_string(quote(do: !(foo > bar))) == "!(foo > bar)"
      assert Macro.to_string(quote(do: @foo(bar))) == "@foo(bar)"
      assert Macro.to_string(quote(do: identity(&1))) == "identity(&1)"
    end

    test "access" do
      assert Macro.to_string(quote(do: a[b])) == "a[b]"
      assert Macro.to_string(quote(do: a[1 + 2])) == "a[1 + 2]"
      assert Macro.to_string(quote(do: (a || [a: 1])[:a])) == "(a || [a: 1])[:a]"
      assert Macro.to_string(quote(do: Map.put(%{}, :a, 1)[:a])) == "Map.put(%{}, :a, 1)[:a]"
    end

    test "keyword list" do
      assert Macro.to_string(quote(do: [a: a, b: b])) == "[a: a, b: b]"
      assert Macro.to_string(quote(do: [a: 1, b: 1 + 2])) == "[a: 1, b: 1 + 2]"
      assert Macro.to_string(quote(do: ["a.b": 1, c: 1 + 2])) == "[\"a.b\": 1, c: 1 + 2]"
    end

    test "interpolation" do
      assert Macro.to_string(quote(do: "foo#{bar}baz")) == ~S["foo#{bar}baz"]
    end
end
