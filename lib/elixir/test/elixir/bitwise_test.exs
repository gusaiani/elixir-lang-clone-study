Code.require_file("test_helper.exs", __DIR__)

defmodule Bitwise.FunctionsTest do
  use ExUnit.Case, async: true

  doctest Bitwise, import: true

  use Bitwise, skip_operators: true

  test "bnot/1" do
    assert bnot(1) == -2
  end
end
