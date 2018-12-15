defmodule ExUnit.CaseTemplate do
  @moduledoc """
  This module allows a developer to define a test case
  template to be used throughout their tests. This is useful
  when there are a set of functions that should be shared
  between tests or a set of setup callbacks.

  By using this module, the callbacks and assertions
  available for regular test cases will also be available.

  ## Example

      defmodule MyCase do
        use ExUnit.CaseTemplate

        setup do
          IO.puts "This will run before each test that uses this case"
        end
      end

      defmodule MyTest do
        use MyCase, async: true

        test "truth" do
          assert true
        end
      end

  """

  @doc false
  defmacro __using__(_) do
    quote do
      use ExUnit.Callbacks
    end
  end
end
