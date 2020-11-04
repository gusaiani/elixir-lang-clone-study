defmodule ExUnit do
  @moduledoc """
  Unit testing framework for Elixir.

  ## Example

  A basic setup for ExUnit is shown below:

      # File: assertion_test.exs

      # 1) Start ExUnit.
      ExUnit.start()

      # 2) Create a new test module (test case) and use "ExUnit.Case".
      defmodule AssertionTest do
        # 3) Note that we pass "async: true", this runs the test case
        #    concurrently with other test cases. The individual tests
        #    within each test case are still run serially.
      end
  """
end
