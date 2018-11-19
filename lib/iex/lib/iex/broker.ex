defmodule IEx.Broker do
  @moduledoc false
  @name IEx.Broker

  @type take_ref :: {takeover_ref :: reference(), server_ref :: reference()}

  use GenServer

  ## Shell Api

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
  def init(:ok)
end
