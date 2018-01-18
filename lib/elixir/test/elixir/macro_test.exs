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
  end
end
