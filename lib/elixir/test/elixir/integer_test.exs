Code.require_file("test_helper.exs", __DIR__)

defmodule IntegerTest do
  use ExUnit.Case, async: true

  doctest Integer

  require Integer

  def test_is_odd_in_guards(number) when Integer.is_odd(number), do: number
  def test_is_odd_in_guards(atom) when is_atom(atom) and not Integer.is_odd(atom), do: :atom
  def test_is_odd_in_guards(_number), do: false

  def test_is_even_in_guards(number) when Integer.is_even(number), do: number
  def test_is_even_in_guards(atom) when is_atom(atom) and not Integer.is_even(atom), do: :atom
  def test_is_even_in_guards(_number), do: false

  test "is_odd/1" do
    assert Integer.is_odd(0) == false
    assert Integer.is_odd(1) == true
    assert Integer.is_odd(2) == false
    assert Integer.is_odd(3) == true
    assert Integer.is_odd(-1) == true
    assert Integer.is_odd(-2) == false
    assert Integer.is_odd(-3) == true
    assert test_is_odd_in_guards(10) == false
    assert test_is_odd_in_guards(11) == 11
    assert test_is_odd_in_guards(:not_integer) == :atom
  end

  test "is_even/1" do
    assert Integer.is_even(0) == true
    assert Integer.is_even(1) == false
    assert Integer.is_even(2) == true
    assert Integer.is_even(3) == false
    assert Integer.is_even(-1) == false
    assert Integer.is_even(-2) == true
    assert Integer.is_even(-3) == false
    assert test_is_even_in_guards(10) == 10
    assert test_is_even_in_guards(11) == false
    assert test_is_even_in_guards(:not_integer) == :atom
  end

  test "mod/2" do
    assert Integer.mod(3, 2) == 1
    assert Integer.mod(0, 10) == 0
    assert Integer.mod(30000, 2001) == 1986
    assert Integer.mod(-20, 11) == 2
  end

  test "mod/2 raises ArithmeticError when divisor is 0" do
    assert_raise ArithmeticError, fn -> Integer.mod(3, 0) end
    assert_raise ArithmeticError, fn -> Integer.mod(-50, 0) end
  end

  test "mod/2 raises ArithmeticError when non-integers used as arguments" do
    assert_raise ArithmeticError, fn -> Integer.mod(3.0, 2) end
    assert_raise ArithmeticError, fn -> Integer.mod(20, 1.2) end
  end

  test "floor_div/2" do
    assert Integer.floor_div(3, 2) == 1
    assert Integer.floor_div(0, 10) == 0
    assert Integer.floor_div(30000, 2001) == 14
    assert Integer.floor_div(-20, 11) == -2
  end

  test "floor_div/2 raises ArithmeticError when divisor is 0" do
    assert_raise ArithmeticError, fn -> Integer.floor_div(3, 0) end
    assert_raise ArithmeticError, fn -> Integer.floor_div(-50, 0) end
  end

  test "floor_div/2 raises ArithmeticError when non-integers used as arguments" do
    assert_raise ArithmeticError, fn -> Integer.floor_div(3.0, 2) end
    assert_raise ArithmeticError, fn -> Integer.floor_div(20, 1.2) end
  end

  test "digits/2" do
    assert Integer.digits(0) == [0]
    assert Integer.digits(0, 2) == [0]
    assert Integer.digits(1) == [1]
    assert Integer.digits(-1) == [-1]
    assert Integer.digits(123, 123) == [1, 0]
    assert Integer.digits(-123, 123) == [-1, 0]
    assert Integer.digits(456, 1000) == [456]
    assert Integer.digits(-456, 1000) == [-456]
    assert Integer.digits(123) == [1, 2, 3]
    assert Integer.digits(-123) == [-1, -2, -3]
    assert Integer.digits(58127, 2) == [1, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1]
    assert Integer.digits(-58127, 2) == [-1, -1, -1, 0, 0, 0, -1, -1, 0, 0, 0, 0, -1, -1, -1, -1]

    for n <- Enum.to_list(-1..1) do
      assert_raise FunctionClauseError, fn ->
        Integer.digits(10, n)
        Integer.digits(-10, n)
      end
    end
  end

end
