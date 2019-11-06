Code.require_file("test_helper.exs", __DIR__)

defmodule ProcessTest do
  use ExUnit.Case, async: true

  doctest Process

  test "dictionary" do
    assert Process.put(:foo, :bar) == nil
    assert Process.put(:foo, :baz) == :bar

    assert Enum.member?(Process.get_keys(), :foo)
    refute Enum.member?(Process.get_keys(), :bar)
    refute Enum.member?(Process.get_keys(), :baz)
    assert Process.get_keys(:bar) == []
    assert Process.get_keys(:baz) == [:foo]

    assert Process.get(:foo) == :baz
    assert Process.delete(:foo) == :baz
    assert Process.get(:foo) == nil
  end

  test "group_leader/2 and group_leader/0" do
    another = spawn_link(fn -> Process.sleep(1000) end)
    assert Process.group_leader(self(), another)
    assert Process.group_leader() == another
  end

  # In contrast with other inlined functions,
  # it is important to test that monitor/1 is inlined,
  # this way we gain the monitor receive optimisation.
  test "monitor/1 is inlined" do
    assert expand(quote(do: Process.monitor(pid())), __ENV__) ==
             quote(do: :erlang.monitor(:process, pid()))
  end
end
