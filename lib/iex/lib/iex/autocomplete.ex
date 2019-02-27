defmodule IEx.Autocomplete do
  @moduledoc false

  @doc """
  Provides one helper function that is injected into connecting
  remote nodes to properly handle autocompletion.
  """
  def remsh(node) do
    fn e ->
      case :rpc.call(node, IEx.Autocomplete, :expand, [e]) do
        {:badrpc, _} -> {:no, '', []}
        r -> r
      end
    end
  end
end
