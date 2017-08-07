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

  @doc false
  defmacro __using__(_) do
    quote do
      @ex_unit_describe nil
      @ex_unit_setup []
      @ex_unit_setup_all []

      @before_compile unquote(__MODULE__)
      import unquote(__MODULE__)
    end
  end

  @doc false
  defmacro __before_compile__(env) do
    [compile_callbacks(env, :setup),
     compile_callbacks(env, :setup_all)]
  end

  @doc """
  Defines a callback to be run before each test in a case.

  ## Examples

      setup :clean_up_tmp_directory

  """
  defmacro setup(block) do
    if Keyword.keyword?(block) do
      do_setup(quote(do: _), block)
    else
      quote do
        @ex_unit_setup ExUnit.Callbacks.__callback__(unquote(block), @ex_unit_describe) ++
                       @ex_unit_setup
      end
    end
  end

  @doc """
  Defines a callback to be run before each test in a case.

  ## Examples

      setup context do
        [conn: Plug.Conn.build_conn()]
      end

  """
  defmacro setup(var, block) do
    do_setup(var, block)
  end

  defp do_setup(var, block) do
    quote bind_quoted: [var: escape(var), block: escape(block)] do
      name = :"__ex_unit_setup_#{length(@ex_unit_setup)}"
      defp unquote(name)(unquote(var)), unquote(block)
      @ex_unit_setup [{name, @ex_unit_describe} | @ex_unit_setup]
    end
  end

  @doc """
  Defines a callback to be run before all tests in a case.

  ## Examples

      setup_all :clean_up_tmp_directory

  """
  defmacro setup_all(block) do
    if Keyword.keyword?(block) do
      do_setup_all(quote(do: _), block)
    else
      quote do
        @ex_unit_describe && raise "cannot invoke setup_all/1 inside describe as setup_all/1 " <>
                                   "always applies to all tests in a module"
        @ex_unit_setup_all ExUnit.Callbacks.__callback__(unquote(block), nil) ++
                           @ex_unit_setup_all
      end
    end
  end

  @doc """
  Defines a callback to be run before all tests in a case.

  ## Examples

      setup_all context do
        [conn: Plug.Conn.build_conn()]
      end

  """
  defmacro setup_all(var, block) do
    do_setup_all(var, block)
  end

  defp do_setup_all(var, block) do
    quote bind_quoted: [var: escape(var), block: escape(block)] do
      @ex_unit_describe && raise "cannot invoke setup_all/2 inside describe"
      name = :"__ex_unit_setup_all_#{length(@ex_unit_setup_all)}"
      defp unquote(name)(unquote(var)), unquote(block)
      @ex_unit_setup_all [{name, nil} | @ex_unit_setup_all]
    end
  end

  @doc """
  Defines a callback that runs once the test exits.

  `callback` is a function that receives no arguments and
  runs in a separate process than the caller.

  `on_exit/2` is usually called from `setup` and `setup_all`
  callbacks, often to undo the action performed during `setup`.
  However, `on_exit/2` may also be called dynamically, where a
  reference can be used to guarantee the callback will be invoked
  only once.
  """
  @spec on_exit(term, (() -> term)) :: :ok | no_return
  def on_exit(name_or_ref \\ make_ref(), callback) when is_function(callback, 0) do
    case ExUnit.OnExitHandler.add(self(), name_or_ref, callback) do
      :ok -> :ok
      :error ->
        raise ArgumentError, "on_exit/2 callback can only be invoked from the test process"
    end
  end
end

