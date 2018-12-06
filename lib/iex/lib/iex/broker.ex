defmodule IEx.Broker do
  @moduledoc false
  @name IEx.Broker

  @type take_ref :: {takeover_ref :: reference(), server_ref :: reference()}

  use GenServer

  ## Shell API

  ## Broker API

  def start_link(_) do
    GenServer.start_link(__MODULE__, :ok, name: @name)
  end

  @doc """
  Registers an IEx server in the broker.

  All instances, except shell ones, are registered.
  """
  @spec register(pid) :: :ok
  def register(pid) do
    GenServer.call(@name, {:register, pid})
  end

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

  def handle_call({:accept, {ref, _server_ref}, group_leader}, {server, _}, state) do
    case pop_in(state.takeovers[ref]) do
      {nil, state} ->
        {:reply, {:error, :already_accepted}, state}

      {{from, _}, state} ->
        GenServer.reply(from, {:ok, server, group_leader})
        {:reply, :ok, state}
    end
  end

  def handle_call({:refuse, {ref, server_ref}}, _from, state) do
    if takeover = state.takeovers[ref] do
      {:reply, {:error, :refused}, refuse(state, ref, takeover, server_ref)}
    else
      {:reply, {:error, :refused}, state}
    end
  end
end
