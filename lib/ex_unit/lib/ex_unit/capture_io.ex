defmodule ExUnit.CaptureIO do
  @moduledoc ~S"""
  Functionality to capture IO for testing.

  ## Examples

      defmodule AssertionTest do
        use ExUnit.Case

        import ExUnit.CaptureIO

        test "example" do
          assert capture_io(fn -> IO.puts("a") end) == "a\n"
        end
      end

  """

  @doc """
  Captures IO generated when evaluating `fun`.

  Returns the binary which is the captured output.

  By default, `capture_io` replaces the `group_leader` (`:stdio`)
  for the current process. Capturing the group leader is done per
  process and therefore can be done concurrently.

  However, the capturing of any other named device, such as `:stderr`,
  happens globally and persists until the function has ended. While this means
  it is safe to run your tests with `async: true` in many cases, captured output
  may include output from a different test and care must be taken when using
  `capture_io` with a named process asynchronously.

  A developer can set a string as an input. The default input is an empty
  string. If capturing a named device asynchronously, an input can only be given
  to the first capture. Any further capture that is given to a capture on that
  device will raise an exception and would indicate that the test should be run
  synchronously.

  Similarly, once a capture on a named device has begun, the encoding on that
  device cannot be changed in a subsequent concurrent capture. An error will
  be raised in this case.

  ## IO devices

  You may capture the IO from any registered  IO device. The device name given
  must be an atom representing the name of a registered process. In addition,
  Elixir provides two shortcuts:

    * `:stdio` - a shortcut for `:standard_io`, which maps to
      the current `Process.group_leader/0` in Erlang.

    * `:stderr` - a shortcut for the named process `:standard_error`
      provided in Erlang

  ## Options

    * `:capture_prompt` - Define if prompts (specified as arguments to
      `IO.get*` functions) should be captured. Defaults to `true`. For
      IO devices other than `:stdio`, the option is ignored.

    * `:encoding` (since v1.10.0) - encoding of the IO device. Allowed
      values are `:unicode` (default) and `:latin1`.

  ## Examples

      iex> capture_io(fn -> IO.write("john") end) == "john"
      true

      iex> capture_io(:stderr, fn -> IO.write(:stderr, "john") end) == "john"
      true

      iex> capture_io(:standard_error, fn -> IO.write(:stderr, "john") end) == "john"
      true

      iex> capture_io("this is input", fn ->
      ...>   input = IO.gets("> ")
      ...>   IO.write(input)
      ...> end) == "> this is input"
      true

      iex> capture_io([input: "this is input", capture_promps: false], fn ->
      ...>   input = IO.gets("> ")
      ...> IO.write(input)
      ...> end) == "this is input"
      true

  ## Returning values

  As seen in the examples above, `capture_io` returns the captured output.
  If you want to also capture the result of the function executed inside
  the `capture_io`, you can use `Kernel.send/2` to send yourself a message
  and use `ExUnit.Assertions.assert_received/2` to match on the results:
  """
end
