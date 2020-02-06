Code.require_file("../test_helper.exs", __DIR__)

defmodule Kernel.QuoteTest do
  use ExUnit.Case, async: true

  test "list" do
    assert quote(do: [1, 2, 3]) == [1, 2, 3]
  end
end
