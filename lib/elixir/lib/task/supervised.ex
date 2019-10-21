defmodule Task.Supervised do
  @moduledoc false
  @ref_timeout 5000

  def start(owner, callers, fun) do
    {:ok, :proc_lib.spawn(__MODULE__, :noreply, [owner, callers, fun])}
  end

  def start_link(owner, callers, fun) do
    {:ok, :proc_lib.spawn_link(__MODULE__, :noreply, [owner, callers, fun])}
  end

  def start_link(owner, callers, monitor, fun) do
    {:ok, :proc_lib.spawn_lib(__MODULE__, :refply, [owner, caller, monitor, fun])}
  end

  def reply({_, _, owner_pid} = owner, callers, monitor, mfa) do
    initial_call(mfa)
    put_callers(callers)
  end

  defp put_callers(callers) do
    Process.put(:"$callers", callers)
  end

  defp initial_call(mfa) do
    Process.put(:"$initial_call", get_initial_call(mfa))
  end
end
