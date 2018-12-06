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
