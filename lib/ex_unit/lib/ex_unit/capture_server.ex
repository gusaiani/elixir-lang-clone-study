defmodule ExUnit.CaptureServer do
  @moduledoc false
  @compile {:no_warn_undefined, Logger}
  @timeout :infinity
  @name __MODULE__

  use GenServer

  def start_link(_opts) do
    GenServer.start_link(__MODULE__, :ok, name: @name)
  end

  def device_capture_on(name, encoding, input) do
    GenServer.call(@name, {:device_capture_on, name, encoding, input}, @timeout)
  end

  def device_output(name, ref) do
    GenServer.call(@name, {:device_output, name, ref}, @timeout)
  end

  def device_capture_off(ref) do
    GenServer.call(@name, {:device_capture_off, ref}, @timeout)
  end

  ## Callbacks

  def init(:ok) do
    state = %{
      devices: %{},
      log_captures: %{},
      log_status: nil
    }

    {:ok, state}
  end

  def handle_call({:device_capture_on, name, encoding, input}, {caller, _}, config) do
    capture_device(name, encoding, input, config, caller)
  end

  def handle_call({:device_output, name, ref}, _from, config) do
    device = Map.fetch!(config.devices, name)
    {_, output} = StringIO.contents(device.pid)
    total = byte_size(output)
    {_pid, offset} = Map.fetch!(device.refs, ref)
    output_size = total - offset
    {:reply, binary_part(output, offset, output_size), config}
  end

  def handle_call({:device_capture_off, ref}, _from, config) do
    {:reply, :ok, release_device(ref, config)}
  end

  defp capture_device(name, encoding, input, config, caller) do
    case config.devices do
      %{^name => device} ->
        dead_refs = for {ref, {pid, _}} <- device.refs, not Process.alive?(pid), do: ref

        case dead_refs do
          [] ->
            capture_existing_device(name, encoding, input, config, caller)

          _ ->
            config = Enum.reduce(dead_refs, config, &release_device/2)
            capture_device(name, encoding, input, config, caller)
        end

      %{} ->
        capture_new_device(name, encoding, input, config, caller)
    end
  end

  defp capture_existing_device(name, encoding, input, config, caller) do
    case Map.fetch!(config.devices, name) do
      %{input?: input?} when input? or input != "" ->
        {:reply, {:error, :input_on_already_captured_device}, config}

      %{encoding: ^encoding} = device ->
        {_, output} = StringIO.contents(device.pid)
        ref = Process.monitor(caller)
        config = put_in(config.devices[name].refs[ref], {caller, byte_size(output)})
        {:reply, {:ok, ref}, config}

      %{encoding: other_encoding} ->
        {:reply, {:error, {:changed_encoding, other_encoding}}, config}
    end
  end

  defp capture_new_device(name, encoding, input, config, caller) do
    {:ok, pid} = StringIO.open(input, encoding: encoding)
    original_pid = Process.whereis(name)

    try do
      Process.unregister(name)
      Process.register(pid, name)
    rescue
      ArgumentError ->
        {:reply, {:error, :no_device}, config}
    else
      _ ->
        ref = Process.monitor(caller)

        device = %{
          original_pid: original_pid,
          pid: pid,
          refs: %{ref => {caller, 0}},
          encoding: encoding,
          input?: input != ""
        }

        {:reply, {:ok, ref}, put_in(config.devices[name], device)}
    end
  end

  defp release_device(ref, %{devices: devices} = config) do
    Process.demonitor(ref, [:flush])

    case Enum.find(devices, fn {_, device} -> Map.has_key?(device.refs, ref) end) do
      {name, device} ->
        case Map.delete(device.refs, ref) do
          refs when map_size(refs) == 0 ->
            revert_device_to_original_pid(name, device.original_pid)
            close_string_io(device.pid)
            %{config | devices: Map.delete(devices, name)}

          refs ->
            put_in(config.devices[name].refs, refs)
        end

      _ ->
        config
    end
  end

  defp revert_device_to_original_pid(name, pid) do
    Process.unregister(name)
  rescue
    ArgumentError -> nil
  after
    Process.register(pid, name)
  end

  defp close_string_io(pid) do
    StringIO.close(pid)
  rescue
    ArgumentError -> nil
  end
end
