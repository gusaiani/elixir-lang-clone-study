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
  over 1 hour! To address such concerns, the VM provides a
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

  defp revision, do: get_revision()

  # Get the date at compilation time.
  defmacrop get_date do
    IO.iodata_to_binary :httpd_util.rfc1123_date
  end

  @doc """
  Returns the endianness.
  """
  def endianness do
    :erlang.system_info(:endian)
  end

  @doc """
  Returns the endianness the system was compiled with.
  """
  @endianness :erlang.system_info(:endian)
  def compiled_endianness do
    @endianness
  end

  @doc """
  Elixir version information.

  Returns Elixir's version as binary.
  """
  @spec version() :: String.t
  def version, do: get_version()

  @doc """
  Elixir build information.

  Returns a keyword list with Elixir version, Git short revision hash and compilation date.
  """
  @spec build_info() :: map
  def build_info do
    %{build:    build(),
      date:     get_date(),
      revision: revision(),
      version:  version()}
  end

  # Returns a string of the build info
  defp build do
    {:ok, v} = Version.parse(version())

    cond do
      ([] == v.pre) or ("" == revision()) ->
        version()
      true ->
        "#{version()} (#{revision()})"
    end
  end

  @doc """
  Lists command line arguments.

  Returns the list of command line arguments passed to the program.
  """
  @spec argv() :: [String.t]
  def argv do
    :elixir_config.get(:argv)
  end

  @doc """
  Modifies command line arguments.

  Changes the list of command line arguments. Use it with caution,
  as it destroys any previous argv information.
  """
  @spec argv([String.t]) :: :ok
  def argv(args) do
    :elixir_config.put(:argv, args)
  end

  @doc """
  Current working directory.

  Returns the current working directory or `nil` if one
  is not available.
  """
  def cwd do
    case :file.get_cwd do
      {:ok, base} -> IO.chardata_to_string(fix_drive_letter(base))
      _ -> nil
    end
  end

  defp fix_drive_letter([l, ?:, ?/ | rest] = original) when l in ?A..?Z do
    case :os.type() do
      {:win32, _} -> [l+?a-?A, ?:, ?/ | rest]
      _ -> original
    end
  end

  defp fix_drive_letter(original), do: original

  @doc """
  Current working directory, exception on error.

  Returns the current working directory or raises `RuntimeError`.
  """
  def cwd! do
    cwd() ||
      raise RuntimeError, message: "could not get a working directory, the current location is not accessible"
  end

  @doc """
  User home directory.

  Returns the user home directory (platform independent).
  """
  def user_home do
    :elixir_config.get(:home)
  end

  @doc """
  User home directory, exception on error.

  Same as `user_home/0` but raises `RuntimeError`
  instead of returning `nil` if no user home is set.
  """
  def user_home! do
    user_home() ||
      raise RuntimeError, message: "could not find the user home, please set the HOME environment variable"
  end

  @doc ~S"""
  Writable temporary directory.

  Returns a writable temporary directory.
  Searches for directories in the following order:

    1. the directory named by the TMPDIR environment variable
    2. the directory named by the TEMP environment variable
    3. the directory name by the TMP environment variable
    4. `C:\TMP` on Window or `/tmp` on Unix
    5. as a last resort, the current working directory

  Returns `nil` if one of the above are writable.
  """
  def tmp_dir do
    write_env_tmp_dir('TMPDIR') ||
      write_env_tmp_dir('TEMP') ||
      write_env_tmp_dir('TMP')  ||
      write_tmp_dir('/tmp')     ||
      ((cwd = cwd()) && write_tmp_dir(cwd))
  end

  @doc """
  Writable temporary directorty, exception on error.

  Same as `tmp_dir/0` but raises `RuntimeError`
  instead of returning `nil` if no temp dir is set.
  """
  def tmp_dir! do
    tmp_dir() ||
      raise RuntimeError, message: "could not get a writable temporary directory, " <>
                                   "please set the TMPDIR environment variable"
  end

  defp write_env_tmp_dir(env) do
    case :os.getenv(env) do
      false -> nil
      tmp   -> write_tmp_dir(tmp)
    end
  end

  defp write_tmp_dir(dir) do
    case File.stat(dir) do
      {:ok, stat} ->
        case {stat.type, stat.access} do
          {:directory, access} when access in [:read_write, :write] ->
            IO.chardata_to_string(dir)
          _ ->
            nil
        end
      {:error, _} -> nil
    end
  end

  
end
