Code.require_file("../test_helper.exs", __DIR__)

defmodule Kernel.ErrorsTest do
  use ExUnit.Case, async: true

  import ExUnit.CaptureIO

  defmacro hello do
    quote location: :keep do
      def hello, do: :world
    end
  end

  test "no optional arguments in fn" do
    assert_eval_raise CompileError,
                      "nofile:1: anonymous functions cannot have optional arguments",
                      'fn x \\\\ 1 -> x end'

    assert_eval_raise CompileError,
                      "nofile:1: anonymous functions cannot have optional arguments",
                      'fn x, y \\\\ 1 -> x + y end'
  end

  test "invalid fn" do
    assert_eval_raise SyntaxError,
                      "nofile:1: expected anonymous functions to be defined with -> inside: 'fn'",
                      'fn 1 end'

    assert_eval_raise SyntaxError,
                      ~r"nofile:2: unexpected operator ->. If you want to define multiple clauses, ",
                      'fn 1\n2 -> 3 end'
  end

  test "invalid token" do
    assert_eval_raise SyntaxError,
                      "nofile:1: unexpected token: \"\u200B\" (column 7, code point U+200B)",
                      '[foo: \u200B]\noops'
  end
end
