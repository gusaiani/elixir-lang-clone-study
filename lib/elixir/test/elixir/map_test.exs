Code.require_file "test_helper.exs", __DIR__

defmodule MapTest do
  use ExUnit.Case, async: true

  doctest Map

  @sample %{a: 1, b: 2}

  test "maps in attributes" do
    assert @sample == %{a: 1, b: 2}
  end

  test "maps when quoted" do
    assert (quote do
      %{foo: 1}
    end) == {:%{}, [], [{:foo, 1}]}
  end

  test "maps keywords and atoms" do
    assert [%{}: :%] == [{:%{}, :%}]
    assert [%: :%{}] == [{:%, :%{}}]
  end

  test "maps with variables" do
    a = 0
    assert %{a: a = 1, b: a} == %{a: 1, b: 0}
    assert a == 1
  end

  test "maps with generated variables in key" do
    assert %{"#{1}" => 1} == %{"1" => 1}
    assert %{(for x <- 1..3, do: x) => 1} == %{[1, 2, 3] => 1}
    assert %{(with x = 1, do: x) => 1} == %{1 => 1}
    assert %{(with {:ok, x} <- {:ok, 1}, do: x) => 1} == %{1 => 1}
    assert %{(try do raise "error" rescue _ -> 1 end) => 1} == %{1 => 1}
    assert %{(try do throw 1 catch x -> x end) => 1} == %{1 => 1}
    assert %{(try do a = 1; a rescue _ -> 2 end) => 1} == %{1 => 1}
    assert %{(try do 1 else a -> a end) => 1} == %{1 => 1}
  end

  test "is_map/1" do
    assert is_map(Map.new)
    refute is_map(Enum.to_list(%{}))
  end

  test "map_size/1" do
    assert map_size(%{}) == 0
    assert map_size(@sample) == 2
  end

  test "take/2" do
    assert Map.take(%{a: 1, b: 2, c: 3}, [:b, :c]) == %{b: 2, c: 3}
    assert Map.take(%{a: 1, b: 2, c: 3}, MapSet.new([:b, :c])) == %{b: 2, c: 3}
    assert Map.take(%{a: 1, b: 2, c: 3}, []) == %{}
    assert_raise BadMapError, fn -> Map.take(:foo, []) end
  end

  test "drop/2" do
    assert Map.drop(%{a: 1, b: 2, c: 3}, [:b, :c]) == %{a: 1}
    assert Map.drop(%{a: 1, b: 2, c: 3}, MapSet.new([:b, :c])) == %{a: 1}
    assert_raise BadMapError, fn -> Map.drop(:foo, []) end
  end

  test "split/2" do
    assert Map.split(%{a: 1, b: 2, c: 3}, [:b, :c]) == {%{b: 2, c: 3}, %{a: 1}}
    assert Map.split(%{a: 1, b: 2, c: 3}, MapSet.new([:b, :c])) == {%{b: 2, c: 3}, %{a: 1}}
    assert_raise BadMapError, fn -> Map.split(:foo, []) end
  end

  test "get_and_update/3" do
    assert_raise RuntimeError, "the given function must return a two-element tuple or :pop, got: 1", fn ->
      Map.get_and_update(%{a: 1}, :a, fn value -> value end)
    end
  end

end
