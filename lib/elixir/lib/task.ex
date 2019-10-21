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

  ## Distributed tasks

  Since Elixir provides a `Task.Supervisor`, it is easy to use one
  to dynamically start tasks across nodes:

      # On the remote node
      Task.Supervisor.start_link(name: MyApp.DistSupervisor)

      # On the client
      supervisor = {MyApp.DistSupervisor, :remote@local}
      Task.Supervisor.async(supervisor, MyMod, :my_fun, [arg1, arg2, arg3])

  Note that, when working with distributed tasks, one should use the `Task.Supervisor.async/4` function
  that expects explicit module, function and arguments, instead of `Task.Supervisor.async/2` that
  works with anonymous functions. That's because anonymous functions expect
  the same module version to exist on all involved nodes. Check the `Agent` module
  documentation for more information on distributed processes as the limitations
  described there apply to the whole ecosystem.

  ## Ancestor and Caller Tracking

  Whenever you start a new process, Elixir annotates the parent of that process
  through the `$ancestors` key in the process dictionary. This is often used to
  track the hierarchy inside a supervision tree.

  For example, we recommend developers to always start tasks under a supervisor.
  This provides more visibility and allows you to control how those tasks are
  terminated when a node shuts down. That might look something like
  `Task.Supervisor.start_child(MySupervisor, task_specification)`. This means
  that, although your code is the one who invokes the task, the actual ancestor of
  the task is the supervisor, as the supervisor is the one effectively starting it.

  To track the relationship between your code and the task, we use the `$callers`
  key in the process dictionary. Therefore, assuming the `Task.Supervisor` call
  above, we have:

      [your code] -- calls --> [supervisor] ---- spawns --> [task]

  Which means we store the following relationships:

      [your code]              [supervisor] <-- ancestor -- [task]
          ^                                                  |
          |--------------------- caller ---------------------|

  The list of callers of the current process can be retrieved from the Process
  dictionary with `Process.get(:"$callers")`. This will return either `nil` or
  a list `[pid_n, ..., pid2, pid1]` with at least one entry where `pid_n` is
  the PID that called the current process, `pid2` called `pid_n`, and `pid2` was
  called by `pid1`.
  """

  @doc """
  The Task struct.

  It contains these fields:

    * `:pid` - the PID of the task process; `nil` if the task does
      not use a task process

    * `:ref` - the task monitor reference

    * `:owner` - the PID of the process that started the task

  """
  @enforce_keys [:pid, :ref, :owner]
  defstruct pid: nil, ref: nil, owner: nil

  @typedoc """
  The Task type.

  See `%Task{}` for information about each field of the structure.
  """
  @type t :: %__MODULE__{
          pid: pid() | nil,
          ref: reference() | nil,
          owner: pid() | nil
        }

  defguardp is_timeout(timeout)
            when timeout == :infinity or (is_integer(timeout) and timeout >= 0)

  @doc """
  Returns a specification to start a task under a supervisor.

  `arg` is passed as the argument to `Task.start_link/1` in the `:start` field
  of the spec.

  For more information, see the `Supervisor` module,
  the `Supervisor.child_spec/2` function and the `t:Supervisor.child_spec/0` type.
  """
  @doc since: "1.5.0"
  @spec child_spec(term) :: Supervisor.child_spec()
  def child_spec(arg) do
    %{
      id: Task,
      start: {Task, :start_link, [arg]},
      restart: :temporary
    }
  end

  @doc false
  defmacro __using__(opts) do
    quote location: :keep, bind_quoted: [opts: opts] do
      unless Module.has_attribute?(__MODULE__, :doc) do
        @doc """
        Returns a specification to start this module under a supervisor.

        `arg` is passed as the argument to `Task.start_link/1` in the `:start` field
        of the spec.

        For more information, see the `Supervisor` module,
        the `Supervisor.child_spec/2` function and the `t:Supervisor.child_spec/0` type.
        """
      end

      def child_spec(arg) do
        default = %{
          id: __MODULE__,
          start: {__MODULE__, :start_link, [arg]},
          restart: :temporary
        }

        Supervisor.child_spec(default, unquote(Macro.escape(opts)))
      end

      defoverridable child_spec: 1
    end
  end

  @doc """
  Starts a process linked to the current process.

  `fun` must be a zero-arity anonymous function.

  This is often used to start the process as part of a supervision tree.
  """
  @spec start_link((() -> any)) :: {:ok, pid}
  def start_link(fun) when is_function(fun, 0) do
    start_link(:erlang, :apply, [fun, []])
  end

  @doc """
  Starts a task as part of a supervision tree.
  """
  @spec start_link(module, atom, [term]) :: {:ok, pid}
  def start_link(module, function_name, args)
      when is_atom(module) and is_atom(function_name) and is_list(args) do
    mfa = {module, function_name, args}
    Task.Supervised.start_link(get_owner(self()), get_callers(self()), mfa)
  end
end
