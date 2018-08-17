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
  developers typically use bastractions such as `Agent`, `GenServer`,
  `Registry`, `Supervisor` and `Task` for building their systems and
  resort to this module for gathering information, trapping exits, links
  and monitoring.
  """

  @doc """
  Stores the given `key`-`value` pair in the process dictionary.

  The return value of this function is the value that was previously stored
  under `key`, or `nil` in case no value was stored under `key`.

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

  @compile {:inline, nillify: 1}
  defp nillify(:undefined), do: nil
  defp nillify(other), do: other
end
