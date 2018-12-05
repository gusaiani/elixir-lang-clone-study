defmodule IEx.Broker do
  @moduledoc false
  @name IEx.Broker

  @type take_ref :: {takeover_ref :: reference(), server_ref :: reference()}

  use GenServer

  ## Shell Api

  @doc """
  Client responds to a takeover request.

  The broker's PID is needed to support remote shells.
  """
  @spec respond(pid, take_ref, boolean()) :: :ok | {:error, :refused | :already_accepted}
  def respond(broker_pid, take_ref, true) do
    GenServer.call(broker_pid, {:accept, take_ref, Process.group_leader()})
  end

  def respond(broker_pid, take_ref, false) do
    GenServer.call(broker_pid, {:refuse, take_ref})
  end

  @doc """
  Client requests a takeover.
  """
  @spec take_over(binary, keyword) ::
          {:ok, server :: pid, group_leader :: pid} | {:error, :no_iex | :refused}
  def take_over(identifier, opts) do
    GenServer.call(@name, {:take_over, identifier, opts}, :infinity)
  end

  ## Callbacks

  @impl true
  def init(:ok) do
    state = %{
      servers: %{},
      takeovers: %{}
    }

    {:ok, state}
  end

  def handle_call({:register, pid}, _from, state) do
    ref = Process.monitor(pid)
    state = put_in(state.servers[ref], pid)
    {:reply, :ok, state}
  end
end
