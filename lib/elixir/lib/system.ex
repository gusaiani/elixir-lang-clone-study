defmodule System do
  @moduledoc """
  The `System` module provides functions that interact directly
  with the VM or the host system.

  ## Time

  The `System` module also provides functions that work with time,
  returning different times kept by the system with support for
  different time units.

  One of the complexities in relying on system times is that they
  may be adjusted. For example, when you enter and leave daylight
  saving time, the system clock will be adjusted, often adding
  or removing one hour. We call such changes "time warps". In
  order to understand how such changes may be harmful, imagine
  the following code:

      ## DO NOT DO THIS
      prev = System.os_time()
      # ... execute some code ...
      next = System.os_time()
      diff = next - prev

  If, while the code is executing, the system clock changes,
  some code that executed in 1 second may be reported as taking
  over 1 hour! To address such concerts, the VM provides a
  monotonic time via `System.monotonic_time/0` which never
  decreases and does not leap:

      ## DO THIS
      prev = System.monotonic_time()
      # ... execute some code ...
      next = System.monotonic_time()
      diff = next - prev

  Generally speaking, the VM provides three time measurements:

    * `os_time/0` - the time reported by the OS. This time may be
      adjusted forwards or backwards in time with no limitation;

    * `system_time/0` - the VM view of the `os_time/0`. The system time and OS
      time may not match in case of time warps although the VM works towards
      aligning them. This time is not monotonic (i.e., it may decrease)
      as its behaviour is configured [by the VM time warp
      mode](http://www.erlang.org/doc/apps/erts/time_correction.html#Time_Warp_Modes);

    * `monotonic_time/0` - a monotonically increasing time provided
      by the Erlang VM.

  The time functions in this module work in the `:native` unit
  (unless specified otherwise), which is OS dependent. Most of
  the time, all calculations are done in the `:native` unit, to
  avoid loss of precision, with `convert_time_unit/3` being
  invoked at the end to convert to a specific time unit like
  `:millisecond` or `:microsecond`. See the `t:time_unit/0` type for
  more information.

  For a more complete rundown on the VM support for different
  times, see the [chapter on time and time
  correction](http://www.erlang.org/doc/apps/erts/time_correction.html)
  in the Erlang docs.
  """

  @typedoc """
  The time unit to be passed to functions like `monotonic_time/1` and others.

  The `:second`, `:millisecond`, `:microsecond` and `:nanosecond` time
  units controls the return value of the functions that accept a time unit.

  A time unit can also be a strictly positive integer. In this case, it
  represents the "parts per second": the time will be returned in `1 /
  parts_per_second` seconds. For example, using the `:millisecond` time unit
  is equivalent to using `1000` as the time unit (as the time will be returned
  in 1/1000 seconds - milliseconds).

  Keep in mind the Erlang API prior to version 19.1 will use `:milli_seconds`,
  `:micro_seconds` and `:nano_seconds` as time units although Elixir normalizes
  their spelling to match the SI convention.
  """
  @type time_unit ::
    :second
    | :millisecond
    | :microsecond
    | :nanosecond
    | pos_integer
    # TODO: Deprecate these in Elixir 2.0
    | :seconds
    | :milliseconds
    | :microseconds
    | :nanoseconds

  @base_dir     :filename.join(__DIR__, "../../..")
  @version_file :filename.join(@base_dir, "VERSION")

  defp strip(iodata) do
    :re.replace(iodata, "^[\s\r\n\t]+|[\s\r\n\t]+$", "", [:global, return: :binary])
  end

  defp read_stripped(path) do
    case :file.read_file(path) do
      {:ok, binary} ->
        strip(binary)
      _ ->
        ""
    end
  end

  # Read and strip the version from the VERSION file.
  defmacrop get_version do
    case read_stripped(@version_file) do
      ""   -> raise RuntimeError, message: "could not read the version number from VERSION"
      data -> data
    end
  end

  # Tries to run "git rev-parse --short HEAD". In the case of success returns
  # the short revision hash. If that fails, returns an empty string.
  defmacrop get_revision do
    null =
      case :os.type do
        {:win32, _} -> 'NUL'
        _ -> '/dev/null'
      end

    'git rev-parse --short HEAD 2> '
    |> Kernel.++(null)
    |> :os.cmd()
    |> strip
  end
end
