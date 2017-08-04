defmodule ExUnit.DuplicateTestError do
  defexception [:message]
end

defmodule ExUnit.DuplicateDescribeError do
  defexception [:message]
end

defmodule ExUnit.Case do
  @moduledoc """
  Helpers for defining test cases.

  This module must be used in other modules as a way to configure
  and prepare them for testing.

  When used, it accepts the following options:

    * `:async` - configure this specific test case to run in parallel
      with other test cases. May be used for performance when this test case
      does not change any global state. Defaults to `false`.

  This module automatically includes all callbacks defined in
  `ExUnit.Callbacks`. See that module for more information on `setup`,
  `start_supervised`, `on_exit` and the test process lifecycle.

  For grouping tests together, see `describe/2` in this module.

  ## Examples

       defmodule AssertionTest do
         # Use the module
         use ExUnit.Case, async: true

         # The "test" macro is imported by ExUnit.Case
         test "always pass" do
           assert true
         end
       end

  ## Context

  All tests receive a context as an argument. The context is particularly
  useful for sharing information between callbacks and tests:

      defmodule KVTest do
        use ExUnit.Case

        setup do
          {:ok, pid} = KV.start_link
          {:ok, pid: pid}
        end

        test "stores key-value pairs", context do
          assert KV.put(context[:pid], :hello, :world) == :ok
          assert KV.get(context[:pid], :hello) == :world
        end
      end

  As the context is a map, it can be pattern matched on to extract
  information:

      test "stores key-value pairs", %{pid: pid} do
        assert KV.put(pid, :hello, :world) == :ok
        assert KV.get(pid, :hello) == :world
      end

  ## Tags

  The context is used to pass information from the callbacks to
  the test. In order to pass information from the test to the
  callback, ExUnit provides tags.

  By tagging a test, the tag value can be accessed in the context,
  allowing the developer to customize the test. Let's see an
  example:

      defmodule FileTest do
        # Changing directory cannot be async
        use ExUnit.Case, async: false

        setup context do
          # Read the :cd tag value
          if cd = context[:cd] do
            prev_cd = File.cwd!
            File.cd!(cd)
            on_exit fn -> File.cd!(prev_cd) end
          end

          :ok
        end

        @tag cd: "fixtures"
        test "reads UTF-8 fixtures" do
          File.read("hello")
        end
      end

  In the example above, we have defined a tag called `:cd` that is
  read in the setup callback to configure the working directory the
  test is going to run on.

  Tags are also very effective when used with case templates
  (`ExUnit.CaseTemplate`) allowing callbacks in the case template
  to customize the test behaviour.

  Note a tag can be set in two different ways:

      @tag key: value
      @tag :key       # equivalent to setting @tag key: true

  If a tag is given more than once, the last value wins.

  ### Module and describe tags

  A tag can be set for all tests in a module or describe block by
  setting `@moduletag` or `@describetag` respectively:

      @moduletag :external

  If the same key is set via `@tag`, the `@tag` value has higher
  precedence.

  ### Known tags

  The following tags are set automatically by ExUnit and are
  therefore reserved:

    * `:case`       - the test case module
    * `:file`       - the file on which the test was defined
    * `:line`       - the line on which the test was defined
    * `:test`       - the test name
    * `:async`      - if the test case is in async mode
    * `:type`       - the type of the test (`:test`, `:property`, etc)
    * `:registered` - used for `ExUnit.Case.register_attribute/3` values
    * `:describe`   - the describe block the test belongs to

  The following tags customize how tests behave:

    * `:capture_log` - see the "Log Capture" section below
    * `:skip` - skips the test with the given reason
    * `:timeout` - customizes the test timeout in milliseconds (defaults to 60000)
    * `:report` - includes the given tags and context keys on error reports,
      see the "Reporting tags" section

  ### Reporting tags

  ExUnit also allows tags or any other key in your context to be included
  in error reports, making it easy for developers to see under which
  circumstances a test was evaluated. To do so, you use the `:report` tag:

      @moduletag report: [:user_id, :server]

  Now when an error happens, there is a tags section containing the value
  for each reported field:

     code: flunk "oops"
     stacktrace:
       lib/my_lib/source.exs:148
     tags:
       user_id: 1
       server: #PID<0.63.0>

  ## Filters

  Tags can also be used to identify specific tests, which can then
  be included or excluded using filters. The most common functionality
  is to exclude some particular tests from running, which can be done
  via `ExUnit.configure/1`:

      # Exclude all external tests from running
      ExUnit.configure(exclude: [external: true])

  From now on, ExUnit will not run any test that has the `external` flag
  set to `true`. This behaviour can be reversed with the `:include` option
  which is usually passed through the command line:

      mix test --include external:true

  Run `mix help test` for more information on how to run filters via Mix.

  Another use case for tags and filters is to exclude all tests that have
  a particular tag by default, regardless of its value, and include only
  a certain subset:

      ExUnit.configure(exclude: :os, include: [os: :unix])

  Keep in mind that all tests are included by default, so unless they are
  excluded first, the `include` option has no effect.

  ## Log Capture

  ExUnit can optionally suppress printing of log messages that are generated
  during a test. Log messages generated while running a test are captured and
  only if the test fails are they printed to aid with debugging.

  You can opt into this behaviour for individual tests by tagging them with
  `:capture_log` or enable log capture for all tests in the ExUnit configuration:

      ExUnit.start(capture_log: true)

  This default can be overridden by `@tag capture_log: false` or
  `@moduletag capture_log: false`.

  Since `setup_all` blocks don't belong to a specific test, log messages generated
  in them (or between tests) are never captured. If you want to suppress these
  messages as well, remove the console backend globally:

      config :logger, backends: []
  """

  @reserved [:case, :file, :line, :test, :async, :registered, :describe, :type]

  @doc false
  defmacro __using__(opts) do
    unless Process.whereis(ExUnit.Server) do
    end
  end

end
