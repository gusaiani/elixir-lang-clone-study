defmodule Supervisor do
  @moduledoc ~S"""
  A behaviour module for implementing supervisors.

  A supervisor is a process which supervises other processes, which we
  refer to as *child processes*. Supervisors are used to build a hierarchical
  process structure called a *supervision tree*. Supervision trees provide
  fault-tolerance and encapsulate how our applications start and shutdown.

  A supervisor implemented using this module has a standard set
  of interface functions and includes functionality for tracing and error
  reporting.

  ## Examples

  In order to define a supervisor, we need to first define a child process
  that will be supervised. As an example, we will define a GenServer that
  represents a stack:

      defmodule Stack do
        use GenServer

        def start_link(state) do
          GenServer.start_link(__MODULE__, state, name: __MODULE__)
        end

        ## Callbacks

        def init(stack) do
          {:ok, stack}
        end

        def handle_call(:pop, _from, [h | t]) do
          {:reply, h, t}
        end

        def handle_cast({:push, h}, t) do
          {:noreply, [h | t]}
        end
      end

  The stack is a small wrapper around lists. It allows us to put
  an element on the top of the stack, by prepending to the list,
  and to get the top of the stack by pattern matching.

  We can now start a supervisor that will start and supervise our
  stack process as follows:

      # Start the supervisor with the stack as a single child.
      #
      # The first element of the tuple is the module containing
      # the child implementation, the second is the argument
      # given to start_link, in this case a stack with `:hello`.
      {:ok, pid} = Supervisor.start_link([
        {Stack, [:hello]}
      ], strategy: :one_for_one)

      # After started, we can query the supervisor for information
      Supervisor.count_children(pid)
      #=> %{active: 1, specs: 1, supervisors: 0, workers: 1}

  Notice that when starting the GenServer, we are registering it
  with name `Stack`, which allows us to call it directly and get
  what is on the stack:

      GenServer.call(Stack, :pop)
      #=> :hello

      GenServer.cast(Stack, {:push, :world})
      #=> :ok

      GenServer.call(Stack, :pop)
      #=> :world

  However, there is a bug in our stack server. If we call `:pop` and
  the stack is empty, it is going to crash because no clause matches:

      GenServer.call(Stack, :pop)
      ** (exit) exited in: GenServer.call(Stack, :pop, 5000)

  Luckily, since the server is being supervised by a supervisor, the
  supervisor will automatically start a new one, with the initial stack
  of `[:hello]`:

      GenServer.call(Stack, :pop)
      #=> :hello

  Supervisors support different strategies; in the example above, we
  have chosen `:one_for_one`. Furthermore, each supervisor can have many
  workers and supervisors as children, each of them with their specific
  configuration, shutdown values, and restart strategies.

  The rest of this document will cover how child processes are started,
  how they can be specified, different supervision strategies and more.

  ## The initialization process

  In the previous section, we have started a supervisor with one child:

      Supervisor.start_link([
        {Stack, [:hello]}
      ], strategy: :one_for_one)

  The first argument given to `start_link` is a list of children.
  In the example above, we have passed a tuple, where the child is
  implemented by the `Stack` module and receives an initial argument
  of `[:hello]` on `Stack.start_link/1`.

  Generally speaking, starting the child process happens in three steps:

    1. First the supervisor calls `Stack.child_spec([:hello])`. This
       function must return a **child specification** which describes
       how the `Stack` process is supervised. When you `use GenServer`,
       a `child_spec/1` is automatically defined for you but we will see
       when and how it can be configured. This function is called once
       when the supervisor starts (or in case of hot code upgrades).

    2. The **child specification** tells the supervisor which function
       to invoke to start the child process. By default, it is the
       `start_link/1` function, receiving the same argument and defined
       in the same module as the `child_spec/1` function. This function
       is called every time a new child process is necessary. For example,
       when we crashed a `Stack` in the previous session,
       `Stack.start_link([:hello])` was called once more to start a new stack.

    3. Finally, `Stack.start_link/1` starts a new process that runs
      `Stack.init/1`, which is responsible for setting a process that
       will react to messages.

  In summary, when the `Supervisor.start_link(children, opts)` is called,
  it traverses the list of children and retrieves their `child_spec/1`.
  Then each child specification describes how each child is started,
  typically via the `start_link/1` function. The supervisor invokes the
  `start_link/1` when it initializes and whenever the child process needs
  to be restarted. The new process started by `start_link/1` often
  executes the `init` callback as its first step. The `init` callback is
  where we initialize and configure the child process.

  ## Child specification

  The child specification describes how the supervisor should start and
  supervise a child process. We have learned that, when we invoked
  `use GenServer`, a `Stack.child_spec/1` was automatically defined for
  us. Let's invoke it and see what it returns:

      Stack.child_spec([:hello])
      #=> %{
        id: Stack,
        start: {Stack, :start_link, [[:hello]]},
        restart: :permanent,
        shutdown: 5000,
        type: :worker
      }

  The child specification contains 5 keys. The first two are required
  and the remaining ones are optional:

    * `:id` - a value used to identify the child specification
      internally by the supervisor; defaults to the given module.
      In case of conflicting `:id`, the supervisor will refuse
      to initialize and require explicit IDs. This key is required.

    * `:start` - a tuple with the module-function-args to be invoked
      to start the child process. This key is required.

    * `:restart` - an atom that defines when a terminated child process
       should be restarted (see the "Restart values" section below).
       This key is optional and defaults to `:permanent`.

    * `:shutdown` - an atom that defines how a child process should be
      terminated (see the "Shutdown values" section below). This key
      is optional and defaults to `5000` if the type is `:worker` or
      `:infinity` if the type is `:supervisor`.

    * `:type` - if the child process is a `:worker` or a `:supervisor`.
      This key is optional and defaults to `:worker`.

  There is a sixth key, called `:modules`, which is rarely changed and
  it is set automatically based on the value in `:start`.

  Most times, the behaviour module you are implementing will take care
  of setting up a proper `child_spec/1` for you. For example, `use Supervisor`
  will define a `child_spec/1` where the `:type` is set to `:supervisor`
  and the `:shutdown` is `:infinity`. Still, if you need to customize
  a certain behaviour, you can do so by defining your own `child_spec/1`
  function or by passing options on `use`. For example, to specify a
  `GenServer` with a shutdown limit of 10 seconds (10_000 miliseconds),
  one might do:

      use GenServer, shutdown: 10_000

  Let's understand what the `:shutdown` and `:restart` options control.

  ### Shutdown values (:shutdown)

  The following shutdown values are supported in the `:shutdown` option:

    * `:brutal_kill` - the child process is unconditionally terminated
      using `Process.exit(child, :kill)`.

    * any integer >= 0 - the amount of time in miliseconds that the
      supervisor will wait for children to terminate after emitting a
      `Process.exit(child, :shutdown)` signal.  If the child process is
      not trapping exits, the initial `:shutdown` signal will terminate
      the child process immediately. If the child process is trapping
      exits, it has the given amount of time in miliseconds to terminate.
      If it doesn't terminate within the specified time, the child process
      is unconditionally terminated by the supervisor via
      `Process.exit(child, :kill)`.

    * `:infinity` - works as an integer except the supervisor will wait
      indefinitely for the child to terminate. If the child process is a
      supervisor, the recommended value is `:infinity` to give the supervisor
      and its children enough time to shutdown. This option can be used with
      regular workers but doing so is discouraged and requires extreme care.
      If not used carefully and the child process does not terminate, it means
      your application will never terminate as well.

  ### Restart values (:restart)

  The `:restart` option controls what the supervisor should consider to
  be a successful termination or not. If the termination is successful,
  the supervisor won't restart the child. If the child process crashed,
  the supervisor will start a new one.
  """
