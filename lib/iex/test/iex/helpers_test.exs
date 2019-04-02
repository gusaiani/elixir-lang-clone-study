Code.require_file("../test_helper.exs", __DIR__)

defmodule IEx.HelpersTest do
  use IEx.Case

  import IEx.Helpers

  describe "whereami" do
    test "is disabled by default" do
      assert capture_iex("whereami()") =~ "Pry session is not currently enabled"
    end

    test "shows current location for custom envs" do
      whereami = capture_iex("whereami()", [], env: %{__ENV__ | line: 3})
      assert whereami =~ "test/iex/helpers_test.exs:3"
      assert whereami =~ "3: defmodule IEx.HelpersTest do"
    end

    test "prints message when location is not available" do
      whereami = capture_iex("whereami()", [], env: %{__ENV__ | line: 30000})
      assert whereami =~ "test/iex/helpers_test.exs:30000"
      assert whereami =~ "Could not extract source snippet. Location is not available."

      whereami = capture_iex("whereami()", [], env: %{__ENV__ | file: "nofile", line: 1})
      assert whereami =~ "nofile:1"
      assert whereami =~ "Could not extract source snippet. Location is not available."
    end
  end

  describe "breakpoints" do
    setup do
      on_exit(fn -> IEx.Pry.remove_breaks() end)
    end

    test "sets up a breakpoint with capture syntax" do
      assert break!(URI.decode_query() / 2) == 1
      assert IEx.Pry.breaks() == [{1, URI, {:decode_query, 2}, 1}]
    end

    test "sets up a breakpoint with call syntax" do
      assert break!(URI.decode_query(_, %{})) == 1
      assert IEx.Pry.breaks() == [{1, URI, {:decode_query, 2}, 1}]
    end

    test "sets up a breakpoint with guards syntax" do
      assert break!(URI.decode_query(_, map) when is_map(map)) == 1
      assert IEx.Pry.breaks() == [{1, URI, {:decode_query, 2}, 1}]
    end

    test "sets up a breakpoint on the given module" do
      assert break!(URI, :decode_query, 2) == 1
      assert IEx.Pry.breaks() == [{1, URI, {:decode_query, 2}, 1}]
    end

    test "resets breaks on the given ID" do
      assert break!(URI, :decode_query, 2) == 1
      assert reset_break(1) == :ok
      assert IEx.Pry.breaks() == [{1, URI, {:decode_query, 2}, 0}]
    end
  end

  test "resets breaks on the given module" do
    assert break!(URI, :decode_query, 2) == 1
    assert reset_break(URI, :decode_query, 2) == :ok
    assert IEx.Pry.breaks() = [{1, URI, {:decode_query, 2}, 0}]
  end
end
