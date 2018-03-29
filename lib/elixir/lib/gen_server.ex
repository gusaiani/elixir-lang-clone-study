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

  Let's start with a code example and the explore the available callbacks.
  Imagine we want a GenServer that works like a stack, allowing us to push
  and pop items:

      defmodule Stack do
        use GenServer

        # Callbacks

        def handle_call(:pop, _from, [h | t]) do
          {:reply, h, t}
        end

        def handle_cast({:push, item}, state) do
          {:no_reply, [item | state]}
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


  """
end
