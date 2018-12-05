defmodule IEx.Server do
  @moduledoc """
  The IEx.Server.

  The server responsibilities include:

    * reading input from the group leader and writing to the group leader
    * sending messages to the evaluator
    * taking over the evaluator process when using `IEx.pry` or setting up breakpoints

  """

  ## Private APIs

  # Starts IEx to run directly from the Erlang shell.
  #
  # The server is spawned only after the callback is done.
  #
  # If there is any takeover during the callback execution
  # we spawn a new server for it without waiting for its
  # conclusion.
  @doc false
  @spec run_from_shell(keyword, {module, atom, [any]}) :: :ok
  def run_from_shell(opts, {m, f, a}) do
    Process.flag(:trap_exit, true)
    {pid, ref} = spawn_monitor(m, f, a)
    shell_loop(opts, pid, ref)
  end

  defp shell_loop(opts, pid, ref) do
    receive do
      {:take_over, take_pid, take_ref, take_identifier, take_opts} ->
        if take_over?
    end
  end

  defp take_over?(take_pid, take_ref, take_identifier) when is_binary(take_identifier) do
    message = IEx.color(:eval_interrupt, "#{take_identifier}\nAllow? [Yn]")
    take_over?(take_pid, take_ref, yes?(IO.gets(:stdio, message)))
  end

  defp take_over?(take_pid, take_ref, take_response) do
    case IEx.Broker.respond(take_pid, take_ref, take_response) do
    end
  end

  defp yes?(string) do
    is_binary(string) and String.trim(string) in ["", "y", "Y", "yes", "YES", "Yes"]
  end
end
