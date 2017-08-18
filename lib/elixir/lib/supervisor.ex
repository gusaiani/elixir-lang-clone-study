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
      #=>   id: Stack,
      #=>   start: {Stack, :start_link, [[:hello]]},
      #=>   restart: :permanent,
      #=>   shutdown: 5000,
      #=>   type: :worker
      #=> }

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
      `Process.exit(child, :shutdown)` signal. If the child process is
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

  The following restart values are supported in the `:restart` option:

    * `:permanent` - the child process is always restarted.

    * `:temporary` - the child process is never restarted, regardless
      of the supervision strategy.

    * `:transient` - the child process is restarted only if it
      terminates abnormally, i.e., with an exit reason other than
      `:normal`, `:shutdown` or `{:shutdown, term}`.

  For a more complete understanding of the exit reasons and their
  impact, see the "Exit reasons" section next.

  ## Exit reasons

  A supervisor restarts a child process depending on its `:restart`
  configuration. For example, when `:restart` is set to `:transient`, the
  supervisor does not restart the child in case it exits with reason `:normal`,
  `:shutdown` or `{:shutdown, term}`.

  So one may ask: which exit reason should I choose when exiting? There are
  three options:

    * `:normal` - in such cases, the exit won't be logged, there is no restart
      in transient mode, and linked processes do not exit

    * `:shutdown` or `{:shutdown, term}` - in such cases, the exit won't be
      logged, there is no restart in transient mode, and linked processes exit
      with the same reason unless they're trapping exits

    * any other term - in such cases, the exit will be logged, there are
      restarts in transient mode, and linked processes exit with the same
      reason unless they're trapping exits

  Notice that supervisor that reached maximum restart intensity will exit with
  `:shutdown` reason. In this case the supervisor will only be restarted if its
  child specification was defined with the `:restart` option set to `:permanent`
  (the default).

  ## Module-based supervisors

  In the example above, a supervisor was started by passing the supervision
  structure to `start_link/2`. However, supervisors can also be created by
  explicitly defining a supervision module:

      defmodule MyApp.Supervisor do
        # Automatically defines child_spec/1
        use Supervisor

        def start_link(arg) do
          Supervisor.start_link(__MODULE__, arg, name: __MODULE__)
        end

        def init(_arg) do
          Supervisor.init([
            {Stack, [:hello]}
          ], strategy: :one_for_one)
        end
      end

  The difference between the two approaches is that a module-based
  supervisor gives you more direct control over how the supervisor
  is initialized. Instead of calling `Supervisor.start_link/2` with
  a list of children that are automatically initialized, we have
  defined a supervisor alongside its `c:init/1` callback and manually
  initialized the children by calling `Supervisor.init/2`, passing
  the same arguments we would have given to `start_link/2`.

  You may want to use a module-based supervisor if:

    * You need to perform some particular action on supervisor
      initialization, like setting up an ETS table.

    * You want to perform partial hot-code swapping of the
      tree. For example, if you add or remove children,
      the module-based supervision will add and remove the
      new children directly, while dynamic supervision
      requires the whole tree to be restarted in order to
      perform such swaps.

  Note `use Supervisor` defines a `child_spec/1` function, allowing
  the defined module to be put under a supervision tree. The generated
  `child_spec/1` can be customized with the following options:

    * `:id` - the child specification id, defaults to the current module
    * `:start` - how to start the child process (defaults to calling `__MODULE__.start_link/1`)
    * `:restart` - when the supervisor should be restarted, defaults to `:permanent`

  ## start_link/2, init/2 and strategies

  So far we have started the supervisor passing a single child as a tuple
  as well as a strategy called `:one_for_one`:

      Supervisor.start_link([
        {Stack, [:hello]}
      ], strategy: :one_for_one)

  Or:

      Supervisor.init([
        {Stack, [:hello]}
      ], strategy: :one_for_one)

  However, children can be specified in three different formats and
  supervisors support different options. Let's formally define those.

  The first argument given to `start_link/2` is a list of children which may
  be either:

    * a module - such as `Stack`. In this case, it is equivalent to passing
      `{Stack, []}` (which means `Stack.child_spec/1` is invoked with an empty
      keywords list)
    * a tuple with a module as first element and the start argument as second -
      such as `{Stack, [:hello]}`. When such format is used, the supervisor
      will retrieve the child specification from the given module.
    * a map representing the child specification itself - such as the child
      specification map outlined in the previous section.

  The second argument is a keyword list of options:

    * `:strategy` - the restart strategy option. It can be either
      `:one_for_one`, `:rest_for_one`, `:one_for_all`, or
      `:simple_one_for_one`. See the "Strategies" section.

    * `:max_restarts` - the maximum amount of restarts allowed in
      a time frame. Defaults to `3`.

    * `:max_seconds` - the time frame in which `:max_restarts` applies.
      Defaults to `5`.

  The `:strategy` option is required and by default a maximum of 3 restarts
  is allowed within 5 seconds.

  ### Strategies

  Supervisors support different supervision strategies (through the
  `:strategy` option, as seen above):

    * `:one_for_one` - if a child process terminates, only that
      process is restarted.

    * `:one_for_all` - if a child process terminates, all other child
      processes are terminated and then all child processes (including
      the terminated one) are restarted.

    * `:rest_for_one` - if a child process terminates, the "rest" of
      the child processes, i.e., the child processes after the terminated
      one in start order, are terminated. Then the terminated child
      process and the rest of the child processes are restarted.

    * `:simple_one_for_one` - similar to `:one_for_one` but suits better
      when dynamically attaching children. This strategy requires the
      supervisor specification to contain only one child. Many functions
      in this module behave slightly differently when this strategy is
      used.

  ## Simple one for one

  The `:simple_one_for_one` supervisor is useful when you want to
  dynamically start and stop supervised children. As an example,
  let's start multiple agents dynamically to keep state.

  One important aspect in `:simple_one_for_one` supervisors is
  that we often want to pass the `:start` arguments later on,
  when starting the children dynamically, rather than when the
  child specification is defined. In such cases, we should not do

      Supervisor.start_link [
        {Agent, fn -> 0 end}
      ]

  as the example above would force all agents to have the same state.
  In such cases, we can use the `child_spec/2` function to build
  and override the fields in a child specification:

      # Override the :start field to have no args.
      # The second argument has no effect thanks to it.
      agent_spec =
        Supervisor.child_spec(Agent, start: {Agent, :start_link, []})

      # We start a supervisor with a simple one for one strategy.
      # The agent won't be started now but later on.
      {:ok, sup_pid} =
        Supervisor.start_link([agent_spec], strategy: :simple_one_for_one)

      # No child worker is active until start_child is called
      Supervisor.count_children(sup_pid)
      #=> %{active: 0, specs: 1, supervisors: 0, workers: 0}

  The simple one for one strategy can define only one child which works
  as a template for when we call `start_child/2`.

  With the supervisor started, let's dynamically start agents:

      {:ok, agent1} = Supervisor.start_child(sup_pid, [fn -> 0 end])
      Agent.update(agent1, & &1 + 1)
      Agent.get(agent1, & &1) #=> 1

      {:ok, agent2} = Supervisor.start_child(sup_pid, [fn -> %{} end])
      Agent.get(agent2, & &1) #=> %{}

      Supervisor.count_children(sup_pid)
      #=> %{active: 2, specs: 1, supervisors: 0, workers: 2}

  ## Name registration

  A supervisor is bound to the same name registration rules as a `GenServer`.
  Read more about these rules in the documentation for `GenServer`.
  """

  @doc false
  defmacro __using__(opts) do
    quote location: :keep, bind_quoted: [opts: opts] do
      @behaviour Supervisor
      import Supervisor.Spec

      spec = [
        id: opts[:id] || __MODULE__,
        start: Macro.escape(opts[:start]) || quote(do: {__MODULE__, :start_link, [arg]}),
        restart: opts[:restart] || :permanent,
        type: :supervisor
      ]

      @doc false
      def child_spec(arg) do
        %{unquote_splicing(spec)}
      end

      defoverridable child_spec: 1

      @doc false
      def init(arg)
    end
  end

  @doc """
  Callback invoked to start the supervisor and during hot code upgrades.
  """
  @callback init(args :: term) ::
    {:ok, {:supervisor.sup_flags, [:supervisor.child_spec]}} |
    :ignore

  @typedoc "Return values of `start_link` functions"
  @type on_start :: {:ok, pid} | :ignore |
                    {:error, {:already_started, pid} | {:shutdown, term} | term}

  @typedoc "Return values of `start_child` functions"
  @type on_start_child :: {:ok, child} | {:ok, child, info :: term} |
                          {:error, {:already_started, child} | :already_present | term}

  @type child :: pid | :undefined

  @typedoc "The Supervisor name"
  @type name :: atom | {:global, term} | {:via, module, term}

  @typedoc "Option values used by the `start*` functions"
  @type option :: {:name, name} | flag()

  @typedoc "Options used by the `start*` functions"
  @type options :: [option, ...]

  @typedoc "The supervisor reference"
  @type supervisor :: pid | name | {atom, node}

  @typedoc "Options given to `start_link/2` and `init/2`"
  @type flag :: {:strategy, strategy} |
                {:max_restarts, non_neg_integer} |
                {:max_seconds, pos_integer}

  @typedoc "Supported strategies"
  @type strategy :: :simple_one_for_one | :one_for_one | :one_for_all | :rest_for_one

  # Note we have inlined all types for readability
  @typedoc "The supervisor specification"
  @type child_spec :: %{
    required(:id) => term(),
    required(:start) => {module(), function(), [term()]},
    optional(:restart) => :permanent | :transient | :temporary,
    optional(:shutdown) => :brutal_kill | non_neg_integer() | :infinity,
    optional(:type) => :worker | :supervisor,
    optional(:modules) => [module()] | :dynamic
  }

  @doc """
  Starts a supervisor with the given children.

  The children is a list of modules, 2-element tuples with module and
  arguments or a map with the child specification. A strategy is required
  to be provided through the `:strategy` option. See
  "start_link/2, init/2 and strategies" for examples and other options.

  The options can also be used to register a supervisor name.
  The supported values are described under the "Name registration"
  section in the `GenServer` module docs.

  If the supervisor and its child processes are successfully spawned
  (if the start function of each child process returns `{:ok, child}`,
  `{:ok, child, info}` or `:ignore`) this function returns
  `{:ok, pid}`, where `pid` is the PID of the supervisor. If the supervisor
  is given a name and a process with the specified name already exists,
  the function returns `{:error, }`
  """
