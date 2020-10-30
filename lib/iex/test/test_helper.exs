assert_timeout = String.to_integer(System.get_env("ELIXIR_ASSERT_TIMEOUT") || "500")

System.put_env("ELIXIR_EDITOR", "echo")

{:ok, _} = Application.ensure_all_started(:iex)
IEx.configure(colors: [enabled: false])
ExUnit.start(trace: "--trace" in System.argv(), assert_receive_timeout: assert_timeout)

defmodule IEx.Case do
  use ExUnit.CaseTemplate
end
