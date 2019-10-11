defmodule Task do
  @moduledoc """
  Conveniences for spawning and awaiting tasks.

  Tasks are processes meant to execute one particular
  action throughout their lifetime, often with little or no
  communication with other processes. The most common use case
  for tasks is to convert sequential code into concurrent code
  by computing a value asynchronously:

      task = Task.async(fn -> do_some_work() end)
      res = do_some_other_work()
      res + Task.await(task)

  Tasks spawned with `async` can be awaited on by their caller
  process (and only their caller) as shown in the example above.
  They are implemented by spawning a process that sends a message
  to the caller once the given computation is performed.

  Besides `async/1` and `await/2`, tasks can also be
  started as part of a supervision tree and dynamically spawned
  on remote nodes. We will explore all three scenarios next.

  ## async and await

  One of the common uses of tasks is to convert sequential code
  into concurrent code with `Task.async/1` while keeping its semantics.
  When invoked, a new process will be created, linked and monitored
  by the caller. Once the task action finishes, a message will be sent
  to the caller with the result.

  `Task.await/2` is used to read the message sent by the task.

  There are two important things to consider when using `async`:

    1. If you are using async tasks, you **must await** a reply
       as they are *always* sent. If you are not expecting a reply,
       consider using `Task.start_link/1` detailed below.

    2. async tasks link the caller and the spawned process. This
       means that, if the caller crashes, the task will crash
       too and vice-versa. This is on purpose: if the process
       meant to receive the result no longer exists, there is
       no purpose in completing the computation.

       If this is not desired, use `Task.start/1` or consider starting
       the task under a `Task.Supervisor` using `async_nolink` or
       `start_child`.

  `Task.yield/2` is an alternative to `await/2` where the caller will
  temporarily block, waiting until the task replies or crashes. If the
  result does not arrive within the timeout, it can be called again at a
  later moment. This allows checking for the result of a task multiple
  times. If a reply does not arrive within the desired time,
  `Task.shutdown/2` can be used to stop the task.

  ## Supervised tasks

  It is also possible to spawn a task under a supervisor. The `Task`
  module implements the `child_spec/1` function, which allows it to
  be started directly under a supervisor by passing a tuple with
  a function to run:

      Supervisor.start_link([
        {Task, fn -> :some_work end}
      ], strategy: :one_for_one)

  However, if you want to invoke a specific module, function and
  arguments, or give the task process a name, you need to define
  the task in its own module:

      defmodule MyTask do
        use Task

        def start_link(arg) do
          Task.start_link(__MODULE__, :run, [arg])
        end

        def run(arg) do
          # ...
        end
      end

  And then passing it to the supervisor:

      Supervisor.start_link([
        {MyTask, arg}
      ], strategy: :one_for_one)

  Since these tasks are supervised and not directly linked to
  the caller, they cannot be awaited on. `start_link/1`, unlike
  `async/1`, returns `{:ok, pid}` (which is the result expected
  by supervisors).

  `use Task` defines a `child_spec/1` function, allowing the
  defined module to be put under a supervision tree. The generated
  `child_spec/1` can be customized with the following options:

    * `:id` - the child specification identifier, defaults to the current module
    * `:restart` - when the child should be restarted, defaults to `:temporary`
    * `:shutdown` - how to shut down the child, either immediately or by giving it time to shut down

  Opposite to `GenServer`, `Agent` and `Supervisor`, a Task has
  a default `:restart` of `:temporary`. This means the task will
  not be restarted even if it crashes. If you desire the task to
  be restarted for non-successful exits, do:

      use Task, restart: :transient

  If you want the task to always be restarted:

      use Task, restart: :permanent

  See the "Child specification" section in the `Supervisor` module
  for more detailed information. The `@doc` annotation immediately
  preceding `use Task` will be attached to the generated `child_spec/1`
  function.

  ## Dynamically supervised tasks

  The `Task.Supervisor` module allows developers to dynamically
  create multiple supervised tasks.

  A short example is:

      {:ok, pid} = Task.Supervisor.start_link()

      task =
        Task.Supervisor.async(pid, fn ->
          # Do something
        end)

      Task.await(task)

  However, in the majority of cases, you want to add the task supervisor
  to your supervision tree:

      Supervisor.start_link([
        {Task.Supervisor, name: MyApp.TaskSupervisor}
      ], strategy: :one_for_one)

  Now you can dynamically start supervised tasks:

      Task.Supervisor.start_child(MyApp.TaskSupervisor, fn ->
        # Do something
      end)

  Or even use the async/await pattern:

      Task.Supervisor.async(MyApp.TaskSupervisor, fn ->
        # Do something
      end)
      |> Task.await()

  Finally, check `Task.Supervisor` for other supported operations.
end
