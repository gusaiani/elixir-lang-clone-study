Code.require_file("../test_helper.exs", __DIR__)

defmodule Kernel.WithTest do
  use ExUnit.Case, async: true

  test "basic with" do
    assert with({:ok, res} <- ok(41), do: res) == 41
    assert with(res <- four(), do: res + 10) == 14
  end

  test "matching with" do
    assert with(_..42 <- 1..42, do: :ok) == :ok
    assert with({:ok, res} <- error(), do: res) == :error
    assert with({:ok, _} = res <- ok(42), do: elem(res, 1)) == 42
  end

  test "with guards" do
    assert with(x when x < 2 <- four(), do: :ok) == 4
  end

  defp four() do
    4
  end

  defp error() do
    :error
  end

  defp ok(num) do
    {:ok, num}
  end
end
