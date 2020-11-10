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
        use ExUnit.Case, async: true

        # 4) Use the "test" macro instead of "def" for clarity.
        test "the truth" do
          assert true
        end
      end

  To run the tests above, run the file using `elixir` from the
  command line. Assuming you named the file `assertion_test.exs`,
  you can run it as:

      elixir assertion_test.exs

  ## Case, Callbacks and Assertions

  See `ExUnit.Case` and `ExUnit.Callbacks` for more information
  about defining test cases and setting up callbacks.

  The `ExUnit.Assertions` module contains a set of macros to
  generate assertions with appropriate error messages.

  ## Integration with Mix

  Mix is the project management and build tool for Elixir. Invoking `mix test`
  from the command line will run the tests in each file matching the pattern
  `*_test.exs` found in the `test` directory of your project.

  You must create a `test_helper.exs` file inside the
  `test` directory and put the code common to all tests there.

  The minimum example of a `test_helper.exs` file would be:

      # test/test_helper.exs
      ExUnit.start()

  Mix will load the `test_helper.exs` file before executing the tests.
  It is not necessary to `require` the `test_helper.exs` file in your test
  files. Run `mix help test` for more information.
  """

  @typedoc """
  All tests start with a state of `nil`.

  A finished test can be in one of five states:

    1. Passed (also represented by `nil`)
    2. Failed
    3. Skipped (via @tag :skip)
    4. Excluded (via :exclude filters)
    5. Invalid (when setup_all fails)

  """
  @type state ::
          nil | {:excluded, binary} | {:failed, failed} | {:invalid, module} | {:skipped, binary}

  @typedoc "The error state returned by `ExUnit.Test` and `ExUnit.TestModule`"
  @type failed :: [{Exception.kind(), reason :: term, Exception.stacktrace()}]

  @typedoc "A map representing the results of running a test suite"
  @type suite_result :: %{
          excluded: non_neg_integer,
          failures: non_neg_integer,
          skipped: non_neg_integer,
          total: non_neg_integer
        }

  @type test_id :: {module, name :: atom}

  defmodule Test do
    @moduledoc """
    A struct that keeps information about the test.

    It is received by formatters and contains the following fields:

      * `:name` - the test name
      * `:module` - the test module
      * `:state` - the finished test state (see `t:ExUnit.state/0`)
      * `:time` - the duration in microseconds of the test's runtime
      * `:tags` - the test tags
      * `:logs` - the captured logs

    """
    defstruct [:name, :case, :module, :state, time: 0, tags: %{}, logs: ""]

    # TODO: Remove the `:case` field on v2.0
    @type t :: %__MODULE__{
            name: atom,
            case: module,
            module: module,
            state: ExUnit.state(),
            time: non_neg_integer,
            tags: map,
            logs: String.t()
          }
  end

  defmodule TestModule do
    @moduledoc """
    A struct that keeps information about the test module.

    It is received by formatters and contains the following fields:

      * `:file`  - (since v1.11.0) the file of the test module

      * `:name`  - the test module name

      * `:state` - the test error state (see `t:ExUnit.state/0`)

      * `:tests` - all tests in this module

    """
    defstruct [:file, :name, :state, tests: []]

    @type t :: %__MODULE__{
            file: binary(),
            name: module,
            state: ExUnit.state(),
            tests: [ExUnit.Test.t()]
          }
  end

  defmodule TestCase do
    # TODO: Remove this module on v2.0 (it has been replaced by TestModule)
    @moduledoc false
    defstruct [:name, :state, tests: []]

    @type t :: %__MODULE__{name: module, state: ExUnit.state(), tests: [ExUnit.Test.t()]}
  end

  defmodule TimeoutError do
    defexception [:timeout, :type]

    @impl true
    def message(%timeout: timeout, type: type) do
      """
      #{type} timed out after #{timeout}ms. You can change the timeout:

        1. per test by setting "@tag timeout: x" (accepts :infinity)
        2. per test module by setting "@moduletag timeout: x" (accepts :infinity)
        3. globally via "ExUnit.start(timeout: x)" configuration
        4. by running "mix test --timeout x" which sets timeout
        5. or by running "mix test --trace" which sets timeout to infinity
           (useful when using IEx.pry/0)

      where "x" is the timeout given as integer in milliseconds (defaults to 60_000).
      """
    end
  end

  use Application

  @doc false
  def start(_type, []) do
    children = [
      ExUnit.Server,
      ExUnit.CaptureServer,
      ExUnit.OnExitHandler
    ]

    opts = [strategy: :one_for_one, name: ExUnit.Supervisor]
    Supervisor.start_link(children, opts)
  end

  @doc """
  Starts ExUnit and automatically runs tests right before the
  VM terminates.

  It accepts a set of `options` to configure `ExUnit`
  (the same ones accepted by `configure/1`).

  If you want to run tests manually, you can set the `:autorun` option
  to `false` and use `run/0` to run tests.
  """
  @spec start(Keyword.t()) :: :ok
  def start(options \\ []) do
    {:ok, _} = Application.ensure_all_started(:ex_unit)

    configure(options)
  end

  @doc """
  Configures ExUnit.

  ## Options

  ExUnit supports the following options:

    * `assert_receive_timeout` - the timeout to be used on `assert_receive`
    calls in milliseconds, defaults to `100`;

    *
  """
end
