defmodule Process do
  @moduledoc """
  Conveniences for working with processes and the process dictionary.

  Besides the functions available in this module, the `Kernel` module
  exposes and auto-imports some basic functionality related to processes
  available through the following functions:

    * `Kernel.spawn/1` and `Kernel.spawn/3`
    * `Kernel.spawn_link/1` and `Kernel.spawn_link/3`
    * `Kernel.spawn_monitor/1` and `Kernel.spawn_monitor/3`
    * `Kernel.self/0`
    * `Kernel.send/2`

  While this module provides low-level conveniences to work with processes,
  developers typically use abstractions such as `Agent`, `GenServer`,
  `Registry`, `Supervisor` and `Task` for building their systems and
  resort to this module for gathering information, trapping exits, links
  and monitoring.
  """

  @typedoc """
  A process destination.

  A remote or local PID, a local port, a locally registered name, or a tuple in
  the form of `{registered_name, node}` for a registered name at another node.
  """
  @type dest :: pid | port | (registered_name :: atom) | {registered_name :: atom, node}

  @doc """
  Tells whether the given process is alive on the local node.

  If the process identified by `pid` is alive (that is, it's not exiting and has
  not exited yet) than this function returns `true`. Otherwise, it returns
  `false`.

  `pid` must refer to a process running on the local node or `ArgumentError` is raised.

  Inlined by the compiler.
  """
  @spec alive?(pid) :: boolean
  defdelegate alive?(pid), to: :erlang, as: :is_process_alive

  @doc """
  Returns all key-value pairs in the process dictionary.

  Inlined by the compiler.
  """
  @spec get() :: [{term, term}]
  defdelegate get(), to: :erlang

  @doc """
  Returns the value for the given `key` in the process dictionary,
  or `default` if `key` is not set.

  ## Examples

      # Assuming :locale was not set
      iex> Process.get(:locale, "pt")
      "pt"
      iex> Process.put(:locale, "fr")
      nil
      iex> Process.get(:locale, "pt")
      "fr"

  """
  @spec get(term, default :: term) :: term
  def get(key, default \\ nil) do
    case :erlang.get(key) do
      :undefined -> default
      value -> value
    end
  end

  @doc """
  Returns all keys in the process dictionary.

  Inlined by the compiler.

  ## Examples

      # Assuming :locale was not set
      iex> :locale in Process.get_keys()
      false
      iex> Process.put(:locale, "pt")
      nil
      iex> :locale in Process.get_keys()
      true

  """
  @spec get_keys() :: [term]
  defdelegate get_keys(), to: :erlang

  @doc """
  Returns all keys in the process dictionary that have the given `value`.

  Inlined by the compiler.
  """
  @spec get_keys(term) :: [term]
  defdelegate get_keys(value), to: :erlang

  @doc """
  Stores the given `key`-`value` pair in the process dictionary.

  The return value of this function is the value that was previously stored
  under `key`, or `nil` in case no value was stored under it.

  ## Examples

      # Assuming :locale was not set
      iex> Process.put(:locale, "en")
      nil
      iex> Process.put(:locale, "fr")
      "en"

  """
  @spec put(term, term) :: term | nil
  def put(key, value) do
    nillify(:erlang.put(key, value))
  end

  @doc """
  Deletes the given `key` from the process dictionary.

  Returns the value that was under `key` in the process dictionary,
  or `nil` if `key` was not stored in the process dictionary.

  ## Examples

      iex> Process.put(:comments, ["comment", "other comment"])
      iex> Process.delete(:comments)
      ["comment", "other comment"]
      iex> Process.delete(:comments)
      nil

  """
  @spec delete(term) :: term | nil
  def delete(key) do
    nillify(:erlang.erase(key))
  end

  @doc """
  Sends an exit signal with the given `reason` to `pid`.

  The following behaviour applies if `reason` is any term except `:normal`
  or `:kill`:

    1. If `pid` is not trapping exits, `pid` will exit with the given
       `reason`.

    2. If `pid` is trapping exits, the exit signal is transformed into a
       message `{:EXIT, from, reason}` and delivered to the message queue
       of `pid`.

  If `reason` is the atom `:normal`, `pid` will not exit (unless `pid` is
  the calling process, in which case it will exit with the reason `:normal`).
  If it is trapping exits, the exit signal is transformed into a message
  `{:EXIT, from, :normal}` and delivered to its message queue.

  If `reason` is the atom `:kill`, that is if `Process.exit(pid, :kill)` is called,
  an untrappable exit signal is sent to `pid` which will unconditionally exit
  with reason `:killed`.

  Inlined by the compiler.

  ## Examples

      Process.exit(pid, :kill)
      #=> true

  """
  @spec exit(pid, term) :: true
  defdelegate exit(pid, reason), to: :erlang

  @doc """
  Sleeps the current process for the given `timeout`.

  `timeout` is either the number of milliseconds to sleep as an
  integer or the atom `:infinity`. When `:infinity` is given,
  the current process will sleep forever, and not
  consume or reply to messages.

  **Use this function with extreme care**. For almost all situations
  where you would use `sleep/1` in Elixir, there is likely a
  more correct, faster and precise way of achieving the same with
  message passing.

  For example, if you are waiting for a process to perform some
  action, it is better to communicate the progress of such action
  with messages.

  In other words, **do not**:

      Task.start_link(fn ->
        do_something()
        ...
      end)

      # Wait until work is done
      Process.sleep(2000)

  But **do**:

      parent = self()

      Task.start_link(fn ->
        do_something()
        send(parent, :work_is_done)
        ...
      end)

      receive do
        :work_is_done -> :ok
      after
        # Optional timeout
        30_000 -> :timeout
      end

  For cases like the one above, `Task.async/1` and `Task.await/2` are
  preferred.

  Similarly, if you are waiting for a process to terminate,
  monitor that process instead of sleeping. **Do not**:

      Task.start_link(fn ->
        ...
      end)

      # Wait until task terminates
      Process.sleep(2000)

  Instead **do**:

      {:ok, pid} =
        Task.start_link(fn ->
          ...
        end)

      ref = Process.monitor(pid)

      receive do
        {:DOWN, ^ref, _, _, _} -> :task_is_down
      after
        # Optional timeout
        30_000 -> :timeout
      end

  """
  @spec sleep(timeout) :: :ok
  def sleep(timeout)
      when is_integer(timeout) and timeout >= 0
      when timeout == :infinity do
    receive after: (timeout -> :ok)
  end

  @doc """
  Sends a message to the given `dest`.

  `dest` may be a remote or local PID, a local port, a locally
  registered name, or a tuple in the form of `{registered_name, node}` for a
  registered name at another node.

  Inlined by the compiler.

  ## Options

    * `:noconnect` - when used, if sending the message would require an
      auto-connection to another node the message is not sent and `:noconnect` is
      returned.

    * `:nosuspend` - when used, if sending the message would cause the sender to
      be suspended the message is not sent and `:nosuspend` is returned.

  Otherwise the message is sent and `:ok` is returned.

  ## Examples

      iex> Process.send({:name, :node_that_does_not_exist}, :hi, [:noconnect])
      :noconnect

  """
  @spec send(dest, msg, [option]) :: :ok | :noconnect | :nosuspend
        when dest: dest(),
             msg: any,
             option: :noconnect | :nosuspend
  defdelegate send(dest, msg, options), to: :erlang

  @doc """
  Returns the PID of the group leader for the calling process.

  Inlined by the compiler.

  ## Examples

      Process.group_leader()
      #=> #PID<0.53.0>

  """
  @spec group_leader() :: pid
  defdelegate group_leader(), to: :erlang

  @doc """
  Sets the group leader of the given `pid` to `leader`.

  Typically, this is used when a process started from a certain shell should
  have a group leader other than `:init`.

  Inlined by the compiler.
  """
  @spec group_leader(pid, leader :: pid) :: true
  def group_leader(pid, leader) do
    :erlang.group_leader(leader, pid)
  end

  @compile {:inline, nillify: 1}
  defp nillify(:undefined), do: nil
  defp nillify(other), do: other
end
