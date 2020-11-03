defmodule ExUnit.OnExitHandler do
  @moduledoc false

  @name __MODULE__
  @ets_opts [:public, :named_table, read_concurrency: true, write_concurrency: true]

  # ETS column numbers
  @supervisor 2
  @on_exit 3

  use Agent

  @spec start_link(keyword()) :: {:ok, pid}
  def start_link(_opts) do
    Agent.start_link(fn -> :ets.new(@name, @ets_opts) end, name: @name)
  end

  @spec register(pid) :: :ok
  def register(pid) when is_pid(pid) do
    :ets.insert(@name, {pid, nil, []})
    :ok
  end

  @spec add(pid, term, (() -> term)) :: :ok | :error
  def add(pid, name_or_ref, callback) when is_pid(pid) and is_function(callback, 0) do
    try do
      :ets.lookup_element(@name, pid, @on_exit)
    rescue
      _ -> :error
    else
      entries ->
        entries = List.keystore(entries, name_or_ref, 0, {name_or_ref, callback})
        true = :ets.update_element(@name, pid, {@on_exit, entries})
        :ok
    end
  end

  @spec get_supervisor(pid) :: {:ok, pid | nil} | :error
  def get_supervisor(pid) when is_pid(pid) do
    try do
      {:ok, :ets.lookup_element(@name, pid, @supervisor)}
    rescue
      _ -> :error
    end
  end

  @spec put_supervisor(pid, pid) :: :ok | :error
  def put_supervisor(pid, sup) when is_pid(pid) and is_pid(sup) do
    case :ets.update_element(@name, pid, {@supervisor, sup}) do
      true -> :ok
      false -> :error
    end
  end
end
