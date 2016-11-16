Code.require_file "test_helper.exs", __DIR__

defmodule KernelTest do
  use ExUnit.Case, async: true

  doctest Kernel

  test "=~/2" do
    assert ("abcd" =~ ~r/c(d)/) == true
    assert ("abcd" =~ ~r/e/) == false
    assert ("abcd" =~ ~R/c(d)/) == true
    assert ("abcd" =~ ~R/e/) == false

    string = "^ab+cd*$"
    assert (string =~ "ab+") == true
    assert (string =~ "bb") == false

    assert ("abcd" =~ ~r//) == true
    assert ("abcd" =~ ~R//) == true
    assert ("abcd" =~ "") == true

    assert ("" =~ ~r//) == true
    assert ("" =~ ~R//) == true
    assert ("" =~ "") == true

    assert ("" =~ "abcd") == false
    assert ("" =~ ~r/abcd/) == false
    assert ("" =~ ~R/abcd/) == false

    assert_raise FunctionClauseError, "no function clause matching in Kernel.=~/2", fn ->
      1234 =~ "hello"
    end

    assert_raise FunctionClauseError, "no function clause matching in Kernel.=~/2", fn ->
      1234 =~ ~r"hello"
    end

    assert_raise FunctionClauseError, "no function clause matching in Kernel.=~/2", fn ->
      1234 =~ ~R"hello"
    end

    assert_raise FunctionClauseError, "no function clause matching in Kernel.=~/2", fn ->
      ~r"hello" =~ "hello"
    end

    assert_raise FunctionClauseError, "no function clause matching in Kernel.=~/2", fn ->
      ~r"hello" =~ ~r"hello"
    end

    assert_raise FunctionClauseError, "no function clause matching in Kernel.=~/2", fn ->
      :abcd =~ ~r//
    end

    assert_raise FunctionClauseError, "no function clause matching in Kernel.=~/2", fn ->
      :abcd =~ ""
    end

    assert_raise FunctionClauseError, "no function clause matching in Regex.match?/2", fn ->
      "abcd" =~ nil
    end

    assert_raise FunctionClauseError, "no function clause matching in Regex.match?/2", fn ->
      "abcd" =~ :abcd
    end
  end

  test "^" do
    x = List.first([1])

    assert_raise MatchError, fn ->
      {x, ^x} = {2, 2}
      x
    end
  end

  test "match?/2" do
    a = List.first([0])
    assert match?(b when b > a, 1) == true
    assert binding() == [a: 0]

    assert match?(b when b > a, -1) == false
    assert binding() == [a: 0]
  end

  test "in/2" do
    assert 2 in [1, 2, 3]
    assert 2 in 1..3
    refute 4 in [1, 2, 3]
    refute 4 in 1..3

    list = [1, 2, 3]
    assert 2 in list
    refute 4 in list

    assert 2 in [1 | [2, 3]]
    assert 3 in [1 | list]
  end

  @at_list1 [4, 5]
  @at_range 6..8
  @at_list2 [13, 14]
  def fun_in(x) when x in [0], do: :list
  def fun_in(x) when x in 1..3, do: :range
  def fun_in(x) when x in @at_list1, do: :at_list
  def fun_in(x) when x in @at_range, do: :at_range
  def fun_in(x) when x in [9 | [10, 11]], do: :list_cons
  def fun_in(x) when x in [12 | @at_list2], do: :list_cons_at
  def fun_in(_), do: :none

  test "in/2 in function guard" do
    assert fun_in(0) == :list
    assert fun_in(1) == :range
    assert fun_in(2) == :range
    assert fun_in(3) == :range
    assert fun_in(5) == :at_list
    assert fun_in(6) == :at_range
    assert fun_in(7) == :at_range
    assert fun_in(8) == :at_range
    assert fun_in(9) == :list_cons
    assert fun_in(10) == :list_cons
    assert fun_in(11) == :list_cons
    assert fun_in(12) == :list_cons_at
    assert fun_in(13) == :list_cons_at
    assert fun_in(14) == :list_cons_at

    assert fun_in(0.0) == :none
    assert fun_in(1.0) == :none
    assert fun_in(2.0) == :none
    assert fun_in(3.0) == :none
    assert fun_in(6.0) == :none
    assert fun_in(7.0) == :none
    assert fun_in(8.0) == :none
    assert fun_in(9.0) == :none
    assert fun_in(10.0) == :none
    assert fun_in(11.0) == :none
    assert fun_in(12.0) == :none
    assert fun_in(13.0) == :none
    assert fun_in(14.0) == :none
  end

  def fun_in(x, y, z) when x in y..z, do: true
  def fun_in(_x, _y, _z), do: false

  test "in/2 in dynamic function guard" do
    assert fun_in(1, 1, 3)
    assert fun_in(2, 1, 3)
    assert fun_in(3, 1, 3)

    assert fun_in(1, 3, 1)
    assert fun_in(2, 3, 1)
    assert fun_in(3, 3, 1)

    refute fun_in(0, 1, 3)
    refute fun_in(4, 1, 3)
    refute fun_in(0, 3, 1)
    refute fun_in(4, 3, 1)

    refute fun_in(2, 1.0, 3)
    refute fun_in(2, 1, 3.0)
    refute fun_in(2.0, 1, 3)
  end

  defmacrop case_in(x, y) do
    quote do
      case 0 do
        _ when unquote(x) in unquote(y) -> true
        _ -> false
      end
    end
  end

  test "in/2 in case guard" do
    assert case_in(1, [1, 2, 3]) == true
    assert case_in(1, 1..3) == true
    assert case_in(2, 1..3) == true
    assert case_in(3, 1..3) == true
    assert case_in(-3, -1..-3) == true
  end

  test "in/2 in module body" do
    defmodule InSample do
      @foo [:a, :b]
      true = :a in @foo
    end
  after
    purge(InSample)
  end

  test "in/2 inside and/2" do
    response = %{code: 200}
    if is_map(response) and response.code in 200..299 do
      :pass
    end

    # This module definition copies internal variable
    # defined during in/2 expansion.
    Module.create(InVarCopy, nil, __ENV__)
    purge(InVarCopy)
  end

  test "in/2 with a non-literal non-escaped compile-time range in guards" do
    message = "non-literal range in guard should be escaped with Macro.escape/2"
    assert_raise ArgumentError, message, fn ->
      Code.eval_string """
      defmodule InErrors do
        range = 1..3
        def foo(x) when x in unquote(range), do: :ok
      end
      """
    end
  end

  test "in/2 with a non-compile-time range in guards" do
    message = ~r/invalid args for operator "in", .* got: :hello/
    assert_raise ArgumentError, message, fn ->
      Code.eval_string """
      defmodule InErrors do
        def foo(x) when x in :hello, do: :ok
      end
      """
    end
  end

  test "in/2 with a non-compile-time list cons in guards" do
    message = ~r/invalid args for operator "in", .* got: list\(\)/
    assert_raise ArgumentError, message, fn ->
      Code.eval_string """
      defmodule InErrors do
        def list, do: [1]
        def foo(x) when x in [1 | list()], do: :ok
      end
      """
    end
  end

  test "in/2 with a non-integer range" do
    message = "ranges (first..last) expect both sides to be integers, got: 0..5.0"
    assert_raise ArgumentError, message, fn ->
      last = 5.0
      1 in 0..last
    end
  end

  test "in/2 optimized" do
    assert expand_to_string(quote(do: foo in [])) == "Enum.member?([], foo)"

    result = expand_to_string(quote(do: rand() in 1..2))
    assert result =~ "var = rand()"
    assert result =~ ":erlang.andalso(:erlang.is_integer(var), :erlang.andalso(:erlang.>=(var, 1), :erlang.=<(var, 2)))"

    result = expand_to_string(quote(do: rand() in [1, 2]))
    assert result =~ "var = rand()"
    assert result =~ ":erlang.or(:erlang.=:=(var, 2), :erlang.=:=(var, 1))"

    result = expand_to_string(quote(do: rand() in [1 | [2]]))
    assert result =~ "var = rand()"
    assert result =~ ":erlang.or(:erlang.=:=(var, 1), :erlang.=:=(var, 2))"
  end

  defp expand_to_string(ast) do
    ast
    |> Macro.prewalk(&Macro.expand(&1, __ENV__))
    |> Macro.to_string
  end

  @bitstring <<"foo", 16::4>>

  test "bitstring attribute" do
    assert @bitstring == <<"foo", 16::4>>
  end

  test "paren as nil" do
    assert is_nil(()) == true
    assert (_ = (); ();) == nil
    assert [1, (), 3] == [1, nil, 3]
    assert [do: ()] == [do: nil]
    assert {1, (), 3} == {1, nil, 3}
    assert (Kernel.&& nil, ()) == nil
    assert (() && ()) == nil
    assert (if(() && ()) do
      :ok
    else
      :error
    end) == :error
  end

  test "__info__(:macros)" do
    assert {:in, 2} in Kernel.__info__(:macros)
  end

  test "__info__(:functions)" do
    refute {:__info__, 1} in Kernel.__info__(:functions)
  end

  test "__info__(others)" do
    assert Kernel.__info__(:module) == Kernel
    assert is_list Kernel.__info__(:compile)
    assert is_list Kernel.__info__(:attributes)
    assert is_list Kernel.__info__(:exports)
  end

  def exported?,      do: not_exported?()
  defp not_exported?, do: true

  test "function_exported?/3" do
    assert function_exported?(__MODULE__, :exported?, 0)
    refute function_exported?(__MODULE__, :not_exported?, 0)
  end

  test "macro_exported?/3" do
    assert macro_exported?(Kernel, :in, 2) == true
    assert macro_exported?(Kernel, :def, 1) == true
    assert macro_exported?(Kernel, :def, 2) == true
    assert macro_exported?(Kernel, :def, 3) == false
    assert macro_exported?(Kernel, :no_such_macro, 2) == false
    assert macro_exported?(:erlang, :abs, 1) == false
  end

  test "apply/3 and apply/2" do
    assert apply(Enum, :reverse, [[1 | [2, 3]]]) == [3, 2, 1]
    assert apply(fn x -> x * 2 end, [2]) == 4
  end

  test "binding/0 and binding/1" do
    x = 1
    assert binding() == [x: 1]

    x = 2
    assert binding() == [x: 2]

    y = 3
    assert binding() == [x: 2, y: 3]

    var!(x, :foo) = 4
    assert binding() == [x: 2, y: 3]
    assert binding(:foo) == [x: 4]
  end

  test "binding/0 doesn't warn on underscored vars" do
    _x = 1
    assert binding() == [_x: 1]
  end

  defmodule User do
    assert is_map defstruct name: "john"
  end

  defmodule UserTuple do
    def __struct__({ UserTuple, :ok }) do
      %User{}
    end
  end

  test "struct/1 and struct/2" do
    assert struct(User) == %User{name: "john"}

    user = struct(User, name: "meg")
    assert user == %User{name: "meg"}

    assert struct(user, unknown: "key") == user
    assert struct(user, %{name: "john"}) == %User{name: "john"}
    assert struct(user, name: "other", __struct__: Post) == %User{name: "other"}
  end

  test "struct!/1 and struct!/2" do
    assert struct!(User) == %User{name: "john"}

    user = struct!(User, name: "meg")
    assert user == %User{name: "meg"}

    assert_raise KeyError, fn ->
      struct!(user, unknown: "key")
    end

    assert struct!(user, %{name: "john"}) == %User{name: "john"}
    assert struct!(user, name: "other", __struct__: Post) == %User{name: "other"}
  end

  defdelegate my_flatten(list), to: List, as: :flatten

  dynamic = :dynamic_flatten
  defdelegate unquote(dynamic)(list), to: List, as: :flatten

  test "defdelegate/2" do
    assert my_flatten([[1]]) == [1]
  end

  test "defdelegate/2 with unquote" do
    assert dynamic_flatten([[1]]) == [1]
  end

  test "defdelegate/2 raises with non-variable arguments" do
    msg = "defdelegate/2 only accepts function parameters, got: 1"

    assert_raise ArgumentError, msg, fn -> Code.eval_string("""
      defmodule IntDelegate do
        defdelegate foo(1), to: List
      end
      """, [], __ENV__)
    end

    assert_raise ArgumentError, msg, fn -> Code.eval_string("""
      defmodule IntOptionDelegate do
        defdelegate foo(1 \\\\ 1), to: List
      end
      """, [], __ENV__)
    end
  end

  defdelegate my_reverse(list \\ []), to: :lists, as: :reverse
  defdelegate my_get(map \\ %{}, key, default \\ ""), to: Map, as: :get

  test "defdelegate/2 accepts variable with optional arguments" do
    assert my_reverse() == []
    assert my_reverse([1, 2, 3]) == [3, 2, 1]

    assert my_get("foo") == ""
    assert my_get(%{}, "foo") == ""
    assert my_get(%{"foo" => "bar"}, "foo") == "bar"
    assert my_get(%{}, "foo", "not_found") == "not_found"
  end

  test "get_in/2" do
    users = %{"john" => %{age: 27}, "meg" => %{age: 23}}
    assert get_in(users, ["john", :age]) == 27
    assert get_in(users, ["dave", :age]) == nil
    assert get_in(nil, ["john", :age]) == nil

    map = %{"fruits" => ["banana", "apple", "orange"]}
    assert get_in(map, ["fruits", by_index(0)])  == "banana"
    assert get_in(map, ["fruits", by_index(3)])  == nil
    assert get_in(map, ["unknown", by_index(3)]) == :oops

    assert_raise FunctionClauseError, fn ->
      get_in(users, [])
    end
  end

  test "put_in/3" do
    users = %{"john" => %{age: 27}, "meg" => %{age: 23}}

    assert put_in(users, ["john", :age], 28) ==
           %{"john" => %{age: 28}, "meg" => %{age: 23}}

    assert_raise FunctionClauseError, fn ->
      put_in(users, [], %{})
    end

    assert_raise ArgumentError, "could not put/update key \"john\" on a nil value", fn ->
      put_in(nil, ["john", :age], 28)
    end
  end

  test "put_in/2" do
    users = %{"john" => %{age: 27}, "meg" => %{age: 23}}

    assert put_in(users["john"][:age], 28) ==
           %{"john" => %{age: 28}, "meg" => %{age: 23}}

    assert put_in(users["john"].age, 28) ==
           %{"john" => %{age: 28}, "meg" => %{age: 23}}

    assert_raise BadMapError, fn ->
      put_in(users["dave"].age, 19)
    end

    assert_raise KeyError, fn ->
      put_in(users["meg"].unknown, "value")
    end
  end

  test "update_in/3" do
    users = %{"john" => %{age: 27}, "meg" => %{age: 23}}

    assert update_in(users, ["john", :age], &(&1 + 1)) ==
           %{"john" => %{age: 28}, "meg" => %{age: 23}}

    assert_raise FunctionClauseError, fn ->
      update_in(users, [], fn _ -> %{} end)
    end

    assert_raise ArgumentError, "could not put/update key \"john\" on a nil value", fn ->
      update_in(nil, ["john", :age], fn _ -> %{} end)
    end

    assert_raise UndefinedFunctionError, fn ->
      pop_in(struct(Sample, []), [:name])
    end
  end

  test "update_in/2" do
    users = %{"john" => %{age: 27}, "meg" => %{age: 23}}

    assert update_in(users["john"][:age], &(&1 + 1)) ==
           %{"john" => %{age: 28}, "meg" => %{age: 23}}

    assert update_in(users["john"].age, &(&1 + 1)) ==
           %{"john" => %{age: 28}, "meg" => %{age: 23}}

    assert_raise BadMapError, fn ->
      update_in(users["dave"].age, &(&1 + 1))
    end

    assert_raise KeyError, fn ->
      put_in(users["meg"].unknown, &(&1 + 1))
    end
  end

  def by_index(index) do
    fn
      _, nil, next ->
        next.(:oops)
      :get, data, next ->
        next.(Enum.at(data, index))
      :get_and_update, data, next ->
        current = Enum.at(data, index)
        case next.(current) do
          {get, update} -> {get, List.replace_at(data, index, update)}
          :pop -> {current, List.delete_at(data, index)}
        end
    end
  end
end
