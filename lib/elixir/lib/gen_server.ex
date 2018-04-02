defmodule GenServer do
  @moduledoc """
  A behaviour module for implementing the server of a client-server relation.

  A GenServer is a process like any other Elixir process and it can be used
  to keep state, execute code asynchronously and so on. The advantage of using
  a generic server process (GenServer) implemented using this module is that it
  will have a standard set of interface functions and include functionality for
  tracing and error reporting. It will also fit into a supervision tree.

  ## Example

  The GenServer behaviour abstracts the common client-server interaction.
  Developers are only required to implement the callbacks and functionality
  they are interested in.

  Let's start with a code example and then explore the available callbacks.
  Imagine we want a GenServer that works like a stack, allowing us to push
  and pop items:

      defmodule Stack do
        use GenServer

        # Callbacks

        def handle_call(:pop, _from, [h | t]) do
          {:reply, h, t}
        end

        def handle_cast({:push, item}, state) do
          {:noreply, [item | state]}
        end
      end

      # Start the server
      {:ok, pid} = GenServer.start_link(Stack, [:hello])

      # This is the client
      GenServer.call(pid, :pop)
      #=> :hello

      GenServer.cast(pid, {:push, :world})
      #=> :ok

      GenServer.call(pid, :pop)
      #=> :world

  We start our `Stack` by calling `start_link/2`, passing the module
  with the server implementation and its initial argument (a list
  representing the stack containing the item `:hello`). We can primarily
  interact with the server by sending two types of messages. **call**
  messages expect a reply from the server (and are therefore synchronous)
  while **cast** messages do not.

  Every time you do a `GenServer.call/3`, the client will send a message
  that must be handled by the `c:handle_call/3` callback in the GenServer.
  A `cast/2` message must be handled by `c:handle_cast/2`.

  ## use GenServer and callbacks

  There are 6 callbacks required to be implemented in a `GenServer`. By
  adding `use GenServer` to your module, Elixir will automatically define
  all 6 callbacks for you, leaving it up to you to implement the ones
  you want to customize.

  `use GenServer` also defines a `child_spec/1` function, allowing the
  defined module to be put under a supervision tree. The generated
  `child_spec/1` can be customized with the following options:

    * `:id` - the child specification id, defaults to the current module
    * `:start` - how to start the child process (defaults to calling `__MODULE__.start_link/1`)
    * `:restart` - when the child should be restarted, defaults to `:permanent`
    * `:shutdown` - how to shut down the child

  For example:

      use GenServer, restart: :transient, shutdown: 10_000

  See the `Supervisor` docs for more information.

  ## Name registration

  Both `start_link/3` and `start/3` support the `GenServer` to register
  a name on start via the `:name` option. Registered names are also
  automatically cleaned up on termination. The supported values are:

    * an atom - the GenServer is registered locally with the given name
      using `Process.register/2`.

    * `{:global, term}`- the GenServer is registered globally with the given
      term using the functions in the [`:global` module](http://www.erlang.org/doc/man/global.html).

    * `{:via, module, term}` - the GenServer is registered with the given
      mechanism and name. The `:via` option expects a module that exports
      `register_name/2`, `unregister_name/1`, `whereis_name/1` and `send/2`.
      One such example is the [`:global` module](http://www.erlang.org/doc/man/global.html) which uses these functions
      for keeping the list of names of processes and their associated PIDs
      that are available globally for a network of Elixir nodes. Elixir also
      ships with a local, decentralized and scalable registry called `Registry`
      for locally storing names that are generated dynamically.

  For example, we could start and register our `Stack` server locally as follows:

      # Start the server and register it locally with name MyStack
      {:ok, _} = GenServer.start_link(Stack, [:hello], name: MyStack)

      # Now messages can be sent directly to MyStack
      GenServer.call(MyStack, :pop) #=> :hello

  Once the server is started, the remaining functions in this module (`call/3`,
  `cast/2`, and friends) will also accept an atom, or any `:global` or `:via`
  tuples. In general, the following formats are supported:

    * a `pid`
    * an `atom` if the server is locally registered
    * `{atom, node}` if the server is locally registered at another node
    * `{:global, term}` if the server is globally registered
    * `{:via, module, name}` if the server is registered through an alternative
      registry

  If there is an interest to register dynamic names locally, do not use
  atoms, as atoms are never garbage collected and therefore dynamically
  generated atoms won't be garbage collected. For such cases, you can
  set up your own local registry by using the `Registry` module.

  ## Client / Server APIs

  Although in the example above we have used `GenServer.start_link/3` and
  friends to directly start and communicate with the server, most of the
  time we don't call the `GenServer` functions directly. Instead, we wrap
  the calls in new functions representing the public API of the server.

  Here is a better implementation of our Stack module:

      defmodule Stack do
        use GenServer

        # Client

        def start_link(default) do
          GenServer.start_link(__MODULE__, default)
        end

        def push(pid, item) do
          GenServer.cast(pid, {:push, item})
        end

        def pop(pid) do
          GenServer.call(pid, :pop)
        end

        # Server (callbacks)

        def handle_call(:pop, _from, [h | t]) do
          {:reply, h, t}
        end

        def handle_call(request, from, state) do
          # Call the default implementation from GenServer
          super(request, from, state)
        end

        def handle_cast({:push, item}, state) do
          {:noreply, [item | state]}
        end

        def handle_cast(request, state) do
          super(request, state)
        end
      end

  In practice, it is common to have both server and client functions in
  the same module. If the server and/or client implementations are growing
  complex, you may want to have them in different modules.

  ## Receiving "regular" messages

  The goal of a `GenServer` is to abstract the "receive" loop for developers,
  automatically handling system messages, support code change, synchronous
  calls and more. Therefore, you should never call your own "receive" inside
  the GenServer callbacks as doing so will cause the GenServer to misbehave.

  Besides the synchronous and asynchronous communication provided by `call/3`
  and `cast/2`, "regular" messages sent by functions such as `Kernel.send/2`,
  `Process.send_after/4` and similar, can be handled inside the `c:handle_info/2`
  callback.

  `c:handle_info/2` can be used in many situations, such as handling monitor
  DOWN messaegs sent by `Process.monitor/1`. Another use case for `c:handle_info/2`
  is to perform periodic work, with the help of `Process.send_after/4`:

      defmodule MyApp.Periodically do
        use GenServer

        def start_link do
          GenServer.start_link(__MODULE__, %{})
        end


  """
end
