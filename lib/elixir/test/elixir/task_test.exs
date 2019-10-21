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

  test "can be supervised directly" do
    assert {:ok, _} = Supervisor.start_link([{Task, fn -> :ok end}], strategy: :one_for_one)
  end

  test "generates child_spec/1" do
    defmodule MyTask do
      use Task
    end

    assert MyTask.child_spec([:hello]) == %{
             id: MyTask,
             restart: :temporary,
             start: {MyTask, :start_link, [[:hello]]}
           }

    defmodule CustomTask do
      use Task, id: :id, restart: :permanent, shutdown: :infinity, start: {:foo, :bar, []}
    end

    assert CustomTask.child_spec([:hello]) == %{
             id: :id,
             restart: :permanent,
             shutdown: :infinity,
             start: {:foo, :bar, []}
           }
  end

  test "async/1" do
    parent = self()
    fun = fn -> wait_and_send(parent, :done) end
    task = Task.async(fun)

    # Assert the struct
    assert task.__struct__ == Task
    assert is_pid(task.pid)
    assert is_reference(task.ref)

    # Assert the link
    {:links, links} = Process.info(self(), :links)
    assert task.pid in links
  end
end
