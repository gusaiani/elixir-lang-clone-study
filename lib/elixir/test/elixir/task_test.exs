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

    receive do: (:ready -> :ok)

    # Assert the initial call
    {:name, fun_name} = Function.info(fun, :name)
    assert {__MODULE__, fun_name, 0} === :proc_lib.translate_initial_call(task.pid)

    # Run the task
    send(task.pid, true)

    # Assert response and monitoring messages
    ref = task.ref
    assert_receive {^ref, :done}
    assert_receive {:DOWN, ^ref, _, _, :normal}
  end

  test "async/3" do
    task = Task.async(__MODULE__, :wait_and_send, [self(), :done])
    assert task.__struct__ == Task

    {:links, links} = Process.info(self(), :links)
    assert task.pid in links

    receive do: (:ready -> :ok)
    assert {__MODULE__, :wait_and_send, 2} === :proc_lib.translate_initial_call(task.pid)

    send(task.pid, true)
    assert Task.await(task) === :done
    assert_receive :done
  end

  test "async with $callers" do
    grandparent = self()

    Task.async(fn ->
      parent = self()
      assert Process.get(:"$callers") == [grandparent]

      Task.async(fn ->
        assert Process.get(:"$callers") == [parent, grandparent]
      end)
      |> Task.await()
    end)
    |> Task.await()
  end
end
