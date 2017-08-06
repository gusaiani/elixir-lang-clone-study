defmodule ExUnit.Server do
  @moduledoc false
  @name __MODULE__

  use GenServer

  def start_link(_opts) do
    GenServer.start_link(__MODULE__, :ok, name: @name)
  end

  def add_async_case(name) do
    GenServer.cast(@name, {:add_async_case, name})
  end

  def add_sync_case(name) do
    GenServer.cast(@name, {:add_sync_case, name})
  end

  def cases_loaded do
    GenServer.call(@name, :cases_loaded)
  end

  def take_async_cases(count) do
    timeout = Application.fetch_env!(:ex_unit, :case_load_timeout)
    GenServer.call(@name, {:take_async_cases, count}, timeout)
  end

  def take_sync_cases() do
    timeout = Application.fetch_env!(:ex_unit, :case_load_timeout)
    GenServer.call(@name, :take_sync_cases, timeout)
  end

  ## Callbacks

  def init(:ok) do
    {:ok, %{
      loaded: System.monotonic_time,
      waiting: nil,
      async_cases: [],
      sync_cases: []
    }}
  end

  # Called on demand until we are signaled all cases are loaded
  def handle_call({:take_async_cases, count}, from, %{waiting: nil} = state) do
    {:noreply, take_cases(%{state | waiting: {from, count}})}
  end

  # Called once after all async cases have been sent and reverts the state.
  def handle_call(:take_sync_cases, _from, %{waiting: nil, loaded: :done, async_cases: []} = state) do
    {:reply, state.sync_cases,
     %{state | sync_cases: [], loaded: System.monotonic_time}}
  end

  def handle_call(:cases_loaded, _from, %{loaded: loaded} = state) when is_integer(loaded) do
    diff = System.convert_time_unit(System.monotonic_time - loaded, :native, :microsecond)
    {:reply, diff, take_cases(%{state | loaded: :done})}
  end

  def name do

  end
end
