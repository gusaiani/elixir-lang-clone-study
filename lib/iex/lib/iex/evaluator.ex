defmodule IEx.Evaluator do
  @moduledoc false

  @doc """
  Eval loop for an IEx session. Its responsibilities include:

    * loading of .iex files
    * evaluating code
    * trapping exceptions in the code being evaluated
    * keeping expression history

  """
  def init(command, server, leader, opts) do
    old_leader = Process.group_leader()
    Process.group_leader
  end
end
