defmodule ExUnit.Callbacks do
  @moduledoc ~S"""
  Defines ExUnit callbacks.

  This module defines both `setup_all` and `setup` callbacks, as well as
  the `on_exit/2`, `start_supervised/2` and `stop_supervised/1` functions.

  The setup callbacks are defined via macros and each one can optionally
  receive a map with metadata, usually referred to as `context`. The
  callback may optionally put extra data into the `context` to be used in
  the tests.

  The `setup_all` callbacks are invoked only once per module, before any
  test runs. All `setup` callbacks are run before each test. No callback
  runs if the test case has no tests or all tests have been filtered out.

  `start_supervised/2` is used to start processes under a supervisor. The
  supervisor is linked to the current test process. The supervisor as well
  as all child processes are guaranteed to terminate before any `on_exit/2`
  callback runs.

  `on_exit/2` callbacks are registered on demand, usually to undo an action
  performed by a setup callback. `on_exit/2` may also take a reference,
  allowing callback to be overridden in the future. A registered `on_exit/2`
  callback always runs, while failures in `setup` and `setup_all` will stop
  all remaining setup callbacks from executing.

  Finally, `setup_all` callbacks run in a separate process per module, while
  all `setup` callbacks run in the same process as the test itself. `on_exit/2`
  callbacks always run in a separate process, as implied by their name. The
  test process always exits with reason `:shutdown`, which means any process
  linked to the test process will also exit, although asynchronously. Therefore
  it is preferred to use `start_supervised/2` to guarantee synchronous termination.

  Here is a run down of the life-cycle of the test process:

    1. the test process is spawned
    2. it runs `setup/2` callbacks
    3. it runs the test itself
    4. it stops all supervised processes
    5. the test process exits with reason `:shutdown`
    6. `on_exit/2` callbacks are executed in a separate process

  ## Context

  If you return a keyword list, a map, or `{:ok, keywords | map}` from
  `setup_all`, the keyword list/map will be merged into the current context and
  be available in all subsequent `setup_all`, `setup`, and the `test` itself.

  Similarly, returning a keyword list, map, or `{:ok, keywords | map}` from
  `setup` means that the returned keyword list/map will be merged into the
  current context and be available in all subsequent `setup` and the `test`
  itself.

  Returning `:ok` leaves the context unchanged (both in `setup` and `setup_all`
  callbacks).

  Returning anything else from `setup_all` will force all tests to fail,
  while a bad response from `setup` causes the current test to fail.

  ## Examples

      defmodule AssertionTest do
        use ExUnit.Case, async: true

        # "setup_all" is called once per module before any test runs
        setup_all do
          IO.puts "Starting AssertionTest"

          # No context is returned here
          :ok
        end

        # "setup" is called before each test
        setup do
          IO.puts "This is a setup callback for #{inspect self()}"

          on_exit fn ->
            IO.puts "This is invoked once the test is done. Process: #{inspect self()}"
          end

          # Returns extra metadata to be merged into context
          [hello: "world"]
        end

        # Same as above, but receives the context as argument
        setup context do
          IO.puts "Setting up: #{context.test}"
          :ok
        end

        # Setups can also invoke a local or imported function that returns a context
        setup :invoke_local_or_imported_function

        test "always pass" do
          assert true
        end

        test "uses metadata from setup", context do
          assert context[:hello] == "world"
        end

        defp invoke_local_or_imported_function(context) do
          [from_named_setup: true]
        end
      end
    end

  """
end
