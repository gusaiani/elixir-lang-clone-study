Code.require_file("test_helper.exs", __DIR__)

defmodule ExUnitTest do
  use ExUnit.Case

  import ExUnit.CaptureIO

  test "supports many runs" do
    defmodule SampleTest do
      use ExUnit.Case

      test "true" do
        assert false
      end

      test "false" do
        assert false
      end
    end

    ExUnit.Server.modules_loaded()
    configure_and_reload_on_exit([])

    assert capture_io(fn ->
             assert ExUnit.run() == %{failures: 2, skipped: 0, total: 2, excluded: 0}
           end) =~ "\n2 tests, 2 failures\n"
  end

  test "prints aborted runs on sigquit", config do
    Process.register(self(), :aborted_on_sigquit)
    line = __ENV__.line + 5

    defmodule SleepOnTest do
      use ExUnit.Case, async: true

      test "true" do
        send(:aborted_on_sigquit, :sleeping)
        Process.sleep(:infinity)
      end
    end

    defmodule SleepOnSetupAll do
      use ExUnit.Case, async: true

      setup_all do
        send(:aborted_on_sigquit, :sleeping)
        Process.sleep(:infinity)
      end

      test "true", do: :ok
    end

    ExUnit.Server.modules_loaded()
    configure_and_reload_on_exit(max_cases: 8)

    result =
      capture_io(fn ->
        {pid, ref} = spawn_monitor(fn -> ExUnit.run() end)
        assert_receive :sleeping
        assert_receive :sleeping

        # We are testing implementation details but since this involves
        # the signal handler, it is truly the only way to test it.
        send(pid, {ref, self(), :sigquit})

        receive do
          ^ref -> :ok
        end
      end)

    assert result =~ "Aborting test suite, the following have not completed:"
    assert result =~ ~r"\* ExUnitTest.SleepOnSetupAll \[.*test/ex_unit_test.exs\]"
    assert result =~ ~r"\* test true \[.*test/ex_unit_test.exs:#{line}\]"

    assert result =~ """
           Showing results so far...

           0 failures

           Randomized with seed 0
           """
  end

  test "doesn't hang on exits" do
    defmodule EventServerTest do
      use ExUnit.Case

      test "spawn and crash" do
        spawn_link(fn ->
          exit(:foo)
        end)

        receive after: (1000 -> :ok)
      end
    end

    ExUnit.Server.modules_loaded()
    configure_and_reload_on_exit([])

    assert capture_io(fn ->
             assert ExUnit.run() == %{failures: 1, skipped: 0, total: 1, excluded: 0}
           end) =~ "\n1 test, 1 failure\n"
  end

  test "supports timeouts" do
    defmodule TimeoutTest do
      use ExUnit.Case

      @tag timeout: 10
      test "ok" do
        Process.sleep(:infinity)
      end
    end

    ExUnit.Server.modules_loaded()

    output = capture_io(fn -> ExUnit.run() end)
    assert output =~ "** (ExUnit.TimeoutError) test timed out after 10ms"
    assert output =~ ~r"\(elixir #{System.version()}\) lib/process\.ex:\d+: Process\.sleep/1"
  end

  test "supports configured timeout" do
    defmodule ConfiguredTimeoutTest do
      use ExUnit.Case

      test "ok" do
        Process.sleep(:infinity)
      end
    end

    ExUnit.configure(timeout: 5)
    ExUnit.Server.modules_loaded()
    output = capture_io(fn -> ExUnit.run() end)
    assert output =~ "** (ExUnit.TimeoutError) test timed out after 5ms"
  after
    ExUnit.configure(timeout: 60000)
  end

  test "reports slow tests" do
    defmodule SlowestTest do
      use ExUnit.Case

      test "tardy" do
        refute false
      end

      test "delayed" do
        Process.sleep(5)
        assert false
      end

      test "slowest" do
        Process.sleep(10)
        refute false
      end
    end

    ExUnit.Server.modules_loaded()
    configure_and_reload_on_exit(slowest: 2)

    output = capture_io(fn -> ExUnit.run() end)
    assert output =~ ~r"Top 2 slowest \(\d+\.\d+s\), \d+.\d% of total time:"
    assert output =~ ~r"\* test slowest \(.+ms\)"
    assert output =~ ~r"\* test delayed \(.+ms\)"
  end

  test "sets max cases to one with trace enabled" do
    configure_and_reload_on_exit(trace: true, max_cases: 10)
    config = ExUnit.configuration()
    assert config[:trace]
    assert config[:max_cases] == 1
    assert config[:timeout] == 60000
  end

  test "sets trace when slowest is enabled" do
    configure_and_reload_on_exit(slowest: 10, max_cases: 10)
    config = ExUnit.configuration()
    assert config[:trace]
    assert config[:slowest] == 10
    assert config[:max_cases] == 1
  end

  test "filters to the given test IDs when the :only_test_ids option is provided" do
    defmodule TestIdTestModule do
      use ExUnit.Case

      test "passing", do: :ok
      test "failing", do: assert(1 == 2)
    end

    test_ids =
      MapSet.new([
        {TestIdTestModule, :"test failing"},
        {TestIdTestModule, :"test missing"},
        {MissingModule, :"test passing"}
      ])

    {result, output} = run_with_filter([only_test_ids: test_ids], [])
    assert result == %{failures: 1, skipped: 0, excluded: 0, total: 1}
    assert output =~ "\n1 test, 1 failure\n"
  end

  test "filtering cases with tags" do
    defmodule ParityTest do
      use ExUnit.Case

      test "zero", do: :ok

      @tag even: false
      test "one", do: :ok

      @tag even: true
      test "two", do: assert(1 == 2)

      @tag even: false
      test "three", do: :ok
    end

    # Empty because it is already loaded
    {result, output} = run_with_filter([], [])
    assert result == %{failures: 1, skipped: 0, total: 4, excluded: 0}
    assert output =~ "\n4 tests, 1 failure\n"

    {result, output} = run_with_filter([exclude: [even: true]], [ParityTest])
    assert result == %{failures: 0, skipped: 0, excluded: 1, total: 4}
    assert output =~ "\n4 tests, 0 failures, 1 excluded\n"

    {result, output} = run_with_filter([exclude: :even], [ParityTest])
    assert result == %{failures: 0, skipped: 0, excluded: 3, total: 4}
    assert output =~ "\n4 tests, 0 failures, 3 excluded\n"

    {result, output} = run_with_filter([exclude: :even, include: [even: true]], [ParityTest])
    assert result == %{failures: 1, skipped: 0, excluded: 2, total: 4}
    assert output =~ "\n4 tests, 1 failure, 2 excluded\n"

    {result, output} = run_with_filter([exclude: :test, include: [even: true]], [ParityTest])
    assert result == %{failures: 1, skipped: 0, excluded: 3, total: 4}
    assert output =~ "\n4 tests, 1 failure, 3 excluded\n"
  end

  test "log capturing" do
    defmodule LogCapturingTest do
      use ExUnit.Case

      require Logger

      setup_all do
        :ok = Logger.remove_backend(:console)
        on_exit(fn -> Logger.add_backend(:console, flush: true) end)
      end

      @tag :capture_log
      test "one" do
        Logger.debug("one")
        assert 1 == 1
      end

      @tag :capture_log
      test "two" do
        Logger.debug("two")
        assert 1 == 2
      end

      @tag capture_log: []
      test "three" do
        Logger.debug("three")
        assert 1 == 2
      end

      test "four" do
        Logger.debug("four")
        assert 1 == 2
      end
    end

    ExUnit.Server.modules_loaded()
    output = capture_io(&ExUnit.run/0)
    assert output =~ "[debug] two\n"
    refute output =~ "[debug] one\n"
    assert output =~ "[debug] three\n"
    refute output =~ "[debug] four\n"
  end

  test "supports multi errors" do
    capture_io(:stderr, fn ->
      defmodule MultiTest do
        use ExUnit.Case

        test "multi" do
          error1 =
            try do
              assert 1 = 2
            rescue
              e in ExUnit.AssertionError ->
                {:error, e, __STACKTRACE__}
            end

          error2 =
            try do
              assert 3 > 4
            rescue
              e in ExUnit.AssertionError ->
                {:error, e, __STACKTRACE__}
            end

          raise ExUnit.MultiError, errors: [error1, error2]
        end
      end
    end)

    ExUnit.Server.modules_loaded()
    configure_and_reload_on_exit([])

    output =
      capture_io(fn ->
        assert ExUnit.run() == %{failures: 1, skipped: 0, total: 1, excluded: 0}
      end)

    assert output =~ "\n1 test, 1 failure\n"
    assert output =~ "\n  1) test multi (ExUnitTest.MultiTest)\n"
    assert output =~ "Failure #1\n"
    assert output =~ "Failure #2\n"

    assert_raise ExUnit.MultiError, ~r/oops/, fn ->
      stack =
        try do
          raise("oops")
        rescue
          _ -> __STACKTRACE__
        end

      error = {:error, RuntimeError.exception("oops"), stack}
      raise ExUnit.MultiError, errors: [error]
    end
  end

  test "raises friendly error for duplicate test names" do
    message = ~S("test duplicate" is already defined in ExUnitTest.TestWithSameNames)

    assert_raise ExUnit.DuplicateTestError, message, fn ->
      defmodule TestWithSameNames do
        use ExUnit.Case

        test "duplicate" do
          assert true
        end

        test "duplicate" do
          assert true
        end
      end
    end
  end

  test "produces error on not implemented tests" do
    defmodule TestNotImplemented do
      use ExUnit.Case

      setup context do
        assert context[:not_implemented]
        :ok
      end

      test "this is not implemented yet"
    end

    ExUnit.Server.modules_loaded()
    configure_and_reload_on_exit([])

    output =
      capture_io(fn ->
        assert ExUnit.run() == %{failures: 1, skipped: 0, total: 1, excluded: 0}
      end)

    assert output =~ "Not implemented\n"
    assert output =~ "\n1 test, 1 failure\n"
  end

  test "skips tagged test with skip" do
    defmodule TestSkipped do
      use ExUnit.Case

      setup context do
        assert context[:not_implemented]
        :ok
      end

      @tag :skip
      test "this will raise", do: raise("oops")

      @tag skip: "won't work"
      test "this will also raise", do: raise("oops")
    end

    ExUnit.Server.modules_loaded()
    configure_and_reload_on_exit([])

    output =
      capture_io(fn ->
        assert ExUnit.run() == %{failures: 0, skipped: 2, total: 2, excluded: 0}
      end)

    assert output =~ "\n2 tests, 0 failures, 2 skipped\n"
  end

  test "filtering cases with :case tag" do
    defmodule FirstTestCase do
      use ExUnit.Case
      test "ok", do: :ok
    end

    defmodule SecondTestCase do
      use ExUnit.Case
      test "false", do: assert false
    end

    {result, output} = run_with_filter([exclude: :case], []) # Empty because it is already loaded
    assert result == %{failures: 0, skipped: 2, total: 2}
    assert output =~ "2 tests, 0 failures, 2 skipped"

    {result, output} =
      [exclude: :test, include: [case: "ExUnitTest.SecondTestCase"]]
      |> run_with_filter([FirstTestCase, SecondTestCase])
    assert result == %{failures: 1, skipped: 1, total: 2}
    assert output =~ "1) test false (ExUnitTest.SecondTestCase)"
    assert output =~ "2 tests, 1 failure, 1 skipped"
  end

  test "raises on reserved tag :file in module" do
    assert_raise RuntimeError, "cannot set tag :file because it is reserved by ExUnit", fn ->
      defmodule ReservedTagFile do
        use ExUnit.Case

        @tag file: "oops"
        test "sample", do: :ok
      end
    end
  end

  test "raises on reserved tag :async in module" do
    assert_raise RuntimeError, "cannot set tag :async because it is reserved by ExUnit", fn ->
      defmodule ReservedTagAsync do
        use ExUnit.Case

        @tag async: true
        test "sample", do: :ok
      end
    end
  end

  test "raises on reserved tag :file in setup" do
    defmodule ReservedSetupTagFile do
      use ExUnit.Case

      setup do
        {:ok, file: :foo}
      end

      test "sample", do: :ok
    end

    ExUnit.Server.modules_loaded()

    output = capture_io(fn ->
      assert ExUnit.run() == %{failures: 1, skipped: 0, total: 1}
    end)

    assert output =~ "trying to set reserved field :file"
  end

  test "raises on reserved tag :async in setup" do
    defmodule ReservedSetupTagAsync do
      use ExUnit.Case

      setup do
        {:ok, async: true}
      end

      test "sample", do: :ok
    end

    ExUnit.Server.modules_loaded()

    output = capture_io(fn ->
      assert ExUnit.run() == %{failures: 1, skipped: 0, total: 1}
    end)

    assert output =~ "trying to set reserved field :async"
  end

  test "does not raise on reserved tag in setup_all (lower priority)" do
    defmodule ReservedSetupAllTag do
      use ExUnit.Case

      setup_all do
        {:ok, file: :foo}
      end

      test "sample", do: :ok
    end

    ExUnit.Server.modules_loaded()

    capture_io(fn ->
      assert ExUnit.run() == %{failures: 0, skipped: 0, total: 1}
    end)
  end

  defp run_with_filter(filters, cases) do
    Enum.each(cases, &ExUnit.Server.add_sync_case/1)
    ExUnit.Server.modules_loaded()
    opts = Keyword.merge(ExUnit.configuration, filters)
    output = capture_io fn -> Process.put(:capture_result, ExUnit.Runner.run(opts, nil)) end
    {Process.get(:capture_result), output}
  end

  defp configure_and_reload_on_exit(opts) do
    old_opts = ExUnit.configuration()

    ExUnit.configure(
      Keyword.merge(
        [autorun: false, seed: 0, colors: [enabled: false], exclude: [:exclude]],
        opts
      )
    )

    on_exit(fn -> ExUnit.configure(old_opts) end)
  end
end
