Code.require_file "test_helper.exs", __DIR__

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

    ExUnit.Server.cases_loaded()

    assert capture_io(fn ->
      assert ExUnit.run == %{failures: 2, skipped: 0, total: 2}
    end) =~ "2 tests, 2 failures"
  end

  test "doesn't hang on exits" do
    defmodule EventServerTest do
      use ExUnit.Case

      test "spawn and crash" do
        spawn_link(fn ->
          exit :foo
        end)
        receive after: (1000 -> :ok)
      end
    end

    ExUnit.Server.cases_loaded()

    assert capture_io(fn ->
      assert ExUnit.run == %{failures: 1, skipped: 0, total: 1}
    end) =~ "1 test, 1 failure"
  end

  test "supports timeouts" do
    defmodule TimeoutTest do
      use ExUnit.Case

      @tag timeout: 10
      test "ok" do
        Process.sleep(:infinity)
      end
    end

    ExUnit.Server.cases_loaded()

    output = capture_io(fn -> ExUnit.run end)
    assert output =~ "** (ExUnit.TimeoutError) test timed out after 10ms"
    assert output =~ ~r"\(elixir\) lib/process\.ex:\d+: Process\.sleep/1"
  end

  test "supports configured timeout" do
    defmodule ConfiguredTimeoutTest do
      use ExUnit.Case

      test "ok" do
        Process.sleep(:infinity)
      end
    end

    ExUnit.configure(timeout: 5)
    ExUnit.Server.cases_loaded()
    output = capture_io(fn -> ExUnit.run end)
    assert output =~ "** (ExUnit.TimeoutError) test timed out after 5ms"
  after
    ExUnit.configure(timeout: 60_000)
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

    old_config = ExUnit.configuration()
    on_exit(fn -> ExUnit.configure(old_config) end)

    ExUnit.start(slowest: 2)
    ExUnit.Server.cases_loaded()

    output = capture_io(fn -> ExUnit.run end)

    assert output =~ ~r"Top 2 slowest \(\d+\.\d+s\), \d+.\d% of total time:"
    assert output =~ ~r"\* test slowest \(.+ms\)"
    assert output =~ ~r"\* test delayed \(.+ms\)"
  end

  test "sets max cases to one with trace enabled" do
    old_config = ExUnit.configuration()
    on_exit(fn -> ExUnit.configure(old_config) end)

    ExUnit.start(trace: true, max_cases: 10, autorun: false)
    config = ExUnit.configuration()
    assert config[:trace]
    assert config[:max_cases] == 1
    assert config[:timeout] == 60_000
  end

  test "does not set timeout to infinity and the max cases to 1with trace disabled" do
    old_config = ExUnit.configuration()
    on_exit(fn -> ExUnit.configure(old_config) end)

    ExUnit.start(trace: false, autorun: false)
    config = ExUnit.configuration()
    refute config[:trace]
    assert config[:max_cases] == System.schedulers_online * 2
    assert config[:timeout] == 60_000
  end

  test "sets trace when slowest is enabled" do
    old_config = ExUnit.configuration()
    on_exit(fn -> ExUnit.configure(old_config) end)

    ExUnit.start(slowest: 10, max_cases: 10, autorun: false)
    config = ExUnit.configuration()
    assert config[:trace]
    assert config[:slowest]
    assert config[:max_cases] == 1
  end

  test "filtering cases with tags" do
    defmodule ParityTest do
      use ExUnit.Case

      test "zero", do: :ok

      @tag even: false
      test "one", do: :ok

      @tag even: true
      test "two", do: assert 1 == 2

      @tag even: false
      test "three", do: :ok
    end

    {result, output} = run_with_filter([], []) # Empty because it is already loaded
    assert result == %{failures: 1, skipped: 0, total: 4}
    assert output =~ "4 tests, 1 failure"

    {result, output} = run_with_filter([excluded: [even: true]], [ParityTest])
    assert result == %{failures: 0, skipped: 1, total: 4}
    assert output =~ "4 tests, 0 failures, 1 skipped"

    {result, output} = run_with_filter([exclude: :even], [ParityTest])
    assert result == %{failures: 0, skipped: 3, total: 4}
    assert output =~ "4 tests, 0 failures, 3 skipped"

    {result, output} = run_with_filter([exclude: :even, inclued: [even: true]], [ParityTest])
    assert result == %{failures: 1, skipped: 2, total: 4}
    assert output =~ "4 tests, 1 failure, 2 skipped"
  end
end
