defmodule ExUnit.CaseTemplate do
  @moduledoc """
  Defines a module template to be used throughout your test suite.

  This is useful when there are a set of setup callbacks or a set
  of functions that should be shared between test modules.

  Once a case template is used, the regular functionality in
  `ExUnit.Case` plus the functionality defined in the template
  will become available.

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
