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
  Sends `msg` to `dest` after `time` milliseconds.

  If `dest` is a PID, it must be the PID of a local process, dead or alive.
  If `dest` is an atom, it must be the name of a registered process
  which is looked up at the time of delivery. No error is produced if the name does
  not refer to a process.

  The message is not sent immediately. Therefore, `dest` can receive other messages
  in-between even when `time` is `0`.

  This function returns a timer reference, which can be read with `read_timer/1`
  or canceled with `cancel_timer/1`.

  The timer will be automatically canceled if the given `dest` is a PID
  which is not alive or when the given PID exits. Note that timers will not be
  automatically canceled when `dest` is an atom (as the atom resolution is done
  on delivery).

  Inlined by the compiler.

  ## Options

    * `:abs` - (boolean) when `false`, `time` is treated as relative to the
    current monotonic time. When `true`, `time` is the absolute value of the
    Erlang monotonic time at which `msg` should be delivered to `dest`.
    To read more about Erlang monotonic time and other time-related concepts,
    look at the documentation for the `System` module. Defaults to `false`.

  ## Examples

      timer_ref = Process.send_after(pid, :hi, 1000)

  """
  @spec send_after(pid | atom, term, non_neg_integer, [option]) :: reference
        when option: {:abs, boolean}
  def send_after(dest, msg, time, opts \\ []) do
    :erlang.send_after(time, dest, msg, opts)
  end

  @doc """
  Cancels a timer returned by `send_after/3`.

  When the result is an integer, it represents the time in milliseconds
  left until the timer would have expired.

  When the result is `false`, a timer corresponding to `timer_ref` could not be
  found. This can happen either because the timer expired, because it has
  already been canceled, or because `timer_ref` never corresponded to a timer.

  Even if the timer had expired and the message was sent, this function does not
  tell you if the timeout message has arrived at its destination yet.

  Inlined by the compiler.

  ## Options

    * `:async` - (boolean) when `false`, the request for cancellation is
      synchronous. When `true`, the request for cancellation is asynchronous,
      meaning that the request to cancel the timer is issued and `:ok` is
      returned right away. Defaults to `false`.

    * `:info` - (boolean) whether to return information about the timer being
      cancelled. When the `:async` option is `false` and `:info` is `true`, then
      either an integer or `false` (like described above) is returned. If
      `:async` is `false` and `:info` is `false`, `:ok` is returned. If `:async`
      is `true` and `:info` is `true`, a message in the form `{:cancel_timer,
      timer_ref, result}` (where `result` is an integer or `false` like
      described above) is sent to the caller of this function when the
      cancellation has been performed. If `:async` is `true` and `:info` is
      `false`, no message is sent. Defaults to `true`.

  """
  @spec cancel_timer(reference, options) :: non_neg_integer | false | :ok
        when options: [async: boolean, info: boolean]
  defdelegate cancel_timer(timer_ref, options \\ []), to: :erlang

  @doc """
  Reads a timer created by `send_after/3`.

  When the result is an integer, it represents the time in milliseconds
  left until the timer will expire.

  When the result is `false`, a timer corresponding to `timer_ref` could not be
  found. This can be either because the timer expired, because it has already
  been canceled, or because `timer_ref` never corresponded to a timer.

  Even if the timer had expired and the message was sent, this function does not
  tell you if the timeout message has arrived at its destination yet.

  Inlined by the compiler.
  """
  @spec read_timer(reference) :: non_neg_integer | false
  defdelegate read_timer(timer_ref), to: :erlang

  @type spawn_opt ::
          :link
          | :monitor
          | {:priority, :low | :normal | :high}
          | {:fullsweep_after, non_neg_integer}
          | {:min_heap_size, non_neg_integer}
          | {:min_bin_vheap_size, non_neg_integer}
  @type spawn_opts :: [spawn_opt]

  @doc """
  Spawns the given function according to the given options.

  The result depends on the given options. In particular,
  if `:monitor` is given as an option, it will return a tuple
  containing the PID and the monitoring reference, otherwise
  just the spawned process PID.

  More options are available; for the comprehensive list of available options
  check `:erlang.spawn_opt/4`.

  Inlined by the compiler.

  ## Examples

      Process.spawn(fn -> 1 + 2 end, [:monitor])
      #=> {#PID<0.93.0>, #Reference<0.18808174.1939079169.202418>}
      Process.spawn(fn -> 1 + 2 end, [:link])
      #=> #PID<0.95.0>

  """
  @spec spawn((()-> any), spawn_opts) :: pid | {pid, reference}
  defdelegate spawn(fun, opts), to: :erlang, as: :spawn_opt

  @doc """
  Spawns the given function `fun` from module `mod`, passing the given `args`
  according to the given options.

  The result depends on the given options. In particular,
  if `:monitor` is given as an option, it will return a tuple
  containing the PID and the monitoring reference, otherwise
  just the spawned process PID.

  It also accepts extra options, for the list of available options
  check `:erlang.spawn_opt/4`.

  Inlined by the compiler.
  """
  @spec spawn(module, atom, list, spawn_opts) :: pid | {pid, reference}
  defdelegate spawn(mod, fun, args, opts), to: :erlang, as: :spawn_opt

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
