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
  end

end
