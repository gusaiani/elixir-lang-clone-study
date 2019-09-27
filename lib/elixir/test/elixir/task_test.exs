Code.require_file("test_helper.exs", __DIR__)

defmodule TaskTest do
  use ExUnit.Case
  doctest Task
  @moduletag :capture_log

  def wait_and_send(caller, atom) do
    send(caller, :ready)
    receive do: (true -> true)
    send(caller, atom)
  end

  defp create_task_in_other_process do
    caller = self()
    spawn(fn -> send(caller, Task.async(fn -> nil end)) end)
    receive do: (task -> task)
  end

  defp create_dummy_task(reason) do
    {pid, ref} = spawn_monitor(Kernel, :exit, [reason])

    receive do
      {:DOWN, ^ref, _, _, _} ->
        %Task{ref: ref, pid: pid, owner: self()}
    end
  end

  def sleep(number) do
    Process.sleep(number)
    number
  end
end
