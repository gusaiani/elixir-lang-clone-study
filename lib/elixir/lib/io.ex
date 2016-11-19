defmodule IO do

  @moduledoc """
  Functions handling input/output (IO).

  Many functions in this module expect an IO device as an argument.
  An IO device must be a PID or an atom representing a process.
  For convenience, Elixir provides `:stdio` and `:stderr` as
  shortcuts to Erlang's `:standard_io` and `:standard_error`.

  The majority of the functions expect chardata, i.e. strings or
  lists of characters and string. In case another type is given,
  functions will convert to string via the `String.Chars` protocol
  (as shown in typespecs).

  The functions starting with `bin` expect iodata as an argument,
  i.e. binaries or lists of bytes and binaries.

  ## IO devices

  An IO device may be an atom or a PID. In case it is an atom,
  the atom must be the name of a registered process. In addition,
  Elixir provides two shortcuts:

    * `:stdio` - a shortcut for `:standard_io`, which maps to
      the current `Process.group_leader/0` in Erlang

    * `:stderr` - a shortcut for the named process `:standard_error`
      provided in Erlang.

  IO devices maintain their position, that means subsequent calls to any
  reading or writing functions will start from the place when the device
  was last accessed. Position of files can be changed using the
  `:file.position/2` function.

  """

  @type device :: atom | pid
  @type nodata :: {:error, term} | :eof
  @type chardata() :: :unicode.chardata()

  defmacrop is_iodata(data) do
    quote do
      is_list(unquote(data)) or is_binary(unquote(data))
    end
  end

  @doc """
  Reads from the IO `device`.

  The `device` is iterated by the given number of characters or line by line if
  `:line` is given.
  Alternatively, if `:all` is given, then whole `device` is returned.

  It returns:

    * `data` - the output characters

    * `:eof` - end of file was encountered

    * `{:error, reason}` - other (rare) error condition;
      for instance, `{:error, :estale}` if reading from an
      NFS volume

  If `:all` is given, `:eof` is never returned, but an
  empty string in case the device has reached EOF.
  """
  @spec read(device, :all | :line | non_neg_integer) :: chardata | nodata
  def read(device \\ :stdio, line_or_chars)

  def read(device, :all) do
    do_read_all(map_dev(device), "")
  end

  def read(device, :line) do
    :io.get_line(map_dev(device), '')
  end

  def read(device, count) when is_integer(count) and count >= 0 do
    :io.get_chars(map_dev(device), '', count)
  end

  defp do_read_all(mapped_dev, acc) do
    case :io.get_line(mapped_dev, "") do
      line when is_binary(line) -> do_read_all(mapped_dev, acc <> line)
      :eof -> acc
      other -> other
    end
  end

  @doc """
  Reads from the IO `device`. The operation is Unicode unsafe.

  The `device` is iterated by the given number of bytes or line by line if
  `:line` is given.
  Alternatively, if `:all` is given, the whole `device` is returned.

  It returns:

    * `data` - the output bytes

    * `:eof` - end of file was encountered

    * `{:error, reason}` - other (rare) error condition;
      for instance, `{:error, :estale}` if reading from an
      NFS volume

  If `:all` is given, `:eof` is never returned, but an
  empty string in case the device has reached EOF.

  Note: do not use this function on IO devices in Unicode mode
  as it will return the wrong result.
  """
  @spec binread(device, :all | :line | non_neg_integer) :: iodata | nodata
  def binread(device \\ :stdio, line_or_chars)

  def binread(device, :all) do
    do_binread_all(map_dev(device), "")
  end

  def binread(device, :line) do
    case :file.read_line(map_dev(device)) do
      {:ok, data} -> data
      other -> other
    end
  end

  def binread(device, count) when is_integer(count) and count >= 0 do
    case :file.read(map_dev(device), count) do
      {:ok, data} -> data
      other -> other
    end
  end

  @read_all_size 4096
  defp do_binread_all(mapped_dev, acc) do
    case :file.read(mapped_dev, @read_all_size) do
      {:ok, data} -> do_bin_readall(mapped_dev, acc <> data)
      :eof -> acc
      other -> other
    end
  end

  @doc """
  Writes `item` to the given `device`.

  By default the `device` is the standard output.
  It returns `:ok` if it succeeds.

  ## Examples

      IO.write "sample"
      #=> sample

      IO.write :stderr, "error"
      #=> error

  """
  @spec write(device, chardata | String.Chars.t) :: :ok
  def write(device \\ :stdio, item) do
    :io.put_chars map_dev(device), to_chardata(item)
  end

  @doc """
  Writes `item` as a binary to the given `device`.
  No Unicode conversion happens.
  The operation is Unicode unsafe.

  Check `write/2` for more information.

  Note: do not use this function on IO devices in Unicode mode
  as it will return the wrong result.
  """
  @spec binwrite(device, iodata) :: :ok | {:e}
  def binwrite(device \\ :stdio, item) when is_iodata(item) do
    :file.write map_dev(device), item
  end

  @doc """
  Writes `item` to the given `device`, similar to `write/2`,
  but adds a newline at the end.
  """
  @spec puts(device, chardata | String.Chars.t) :: :ok
  def puts(device \\ :stdio, item) do
    :io.put_chars map_dev(device), [to_chardata(item), ?\n]
  end

  @doc """
  Writes a `message` to stderr, along with the given `stacktrace`.

  This function also notifies the compiler a warning was printed
  (in case --warnings-as-errors was enabled). It returns `:ok`
  if it succeeds.

  An empty list can be passed to avoid stacktrace printing.

  ## Examples

      stacktrace = [{MyApp, :main, [file: 'my_app.ex', line: 4]}]
      IO.warn "variable bar is unused", stacktrace
      #=> warning: variable bar is unused
      #=>   my_app.ex:4: MyApp.main/1

  """
  @spec warn(chardata | String.Chars.t, Exception.stacktrace) :: :ok
  def warn(message, []) do
    :elixir_errors.warn([to_chardata(message), ?\n])
  end
  def warn(message, stacktrace) when is_list(stacktrace) do
    formatted = Enum.map_join(stacktrace, "\n  ", &Exception.format_stacktrace_entry(&1))
    :elixir_errors.warn([to_chardata(message), ?\n, "  ", formatted, ?\n])
  end

  @doc """
  Writes a `message` to stderr, along with the current stacktrace.

  It returns `:ok` if it succeeds.

  ## Examples

      IO.warn "variable bar is unused"
      #=> warning: variable bar is unused
      #=>   (iex) evaluator.ex:108: IEx.Evaluator.eval/4

  """
  @spec warn(chardata | String.Chars.t) :: :ok
  def warn(message) do
    {:current_stacktrace, stacktrace} = Process.info(self(), :current_stacktrace)
    warn(message, Enum.drop(stacktrace, 2))
  end

  # Map the Elixir names for standard IO and error to Erlang names
  defp map_dev(:stdio),  do: :standard_io
  defp map_dev(:stderr), do: :standard_error
  defp map_dev(other) when is_atom(other) or is_pid(other) or is_tuple(other), do: other

  defp to_chardata(list) when is_list(list), do: list
  defp to_chardata(other), do: to_string(other)
end
