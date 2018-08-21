defmodule File do
  @moduledoc ~S"""
  This module contains functions to manipulate files.

  Some of those functions are low-level, allowing the user
  to interact with files or IO devices, like `open/2`,
  `copy/3` and others. This module also provides higher
  level functions that work with filenames and have their naming
  based on UNIX variants. For example, one can copy a file
  via `cp/3` and remove files and directories recursively
  via `rm_rf/1`.

  Paths given to functions in this module can be either relative to the
  current working directory (as returned by `File.cwd/0`), or absolute
  paths. Shell conventions like `~` are not expanded automatically.
  To use paths like `~/Downloads`, you can use `Path.expand/1` or
  `Path.expand/2` to expand your path to an absolute path.

  ## Encoding

  In order to write and read files, one must use the functions
  in the `IO` module. By default, a file is opened in binary mode,
  which requires the functions `IO.binread/2` and `IO.binwrite/2`
  to interact with the file. A developer may pass `:utf8` as an
  option when opening the file, then the slower `IO.read/2` and
  `IO.write/2` functions must be used as they are responsible for
  doing the proper conversions and providing the proper data guarantees.

  Note that filenames when given as charlists in Elixir are
  always treated as UTF-8. In particular, we expect that the
  shell and the operating system are configured to use UTF-8
  encoding. Binary filenames are considered raw and passed
  to the OS as is.

  ## API

  Most of the functions in this module return `:ok` or
  `{:ok, result}` in case of success, `{:error, reason}`
  otherwise. Those functions also have a variant
  that ends with `!` which returns the result (instead of the
  `{:ok, result}` tuple) in case of success or raises an
  exception in case it fails. For example:

      File.read("hello.txt")
      #=> {:ok, "World"}

      File.read("invalid.txt")
      #=> {:error, :enoent}

      File.read!("hello.txt")
      #=> "World"

      File.read!("invalid.txt")
      #=> raises File.Error

  In general, a developer should use the former in case they want
  to react if the file does not exist. The latter should be used
  when the developer expects their software to fail in case the
  file cannot be read (i.e. it is literally an exception).

  ## Processes and raw files

  Every time a file is opened, Elixir spawns a new process. Writing
  to a file is equivalent to sending messages to the process that
  writes to the file descriptor.

  This means files can be passed between nodes and message passing
  guarantees they can write to the same file in a network.

  However, you may not always want to pay the price for this abstraction.
  In such cases, a file can be opened in `:raw` mode. The options `:read_ahead`
  and `:delayed_write` are also useful when operating on large files or
  working with files in tight loops.

  Check `:file.open/2` for more information about such options and
  other performance considerations.
  """

  @type posix :: :file.posix()
  @type io_device :: :file.io_device()
  @type stat_options :: [time: :local | :universal | :posix]
  @type mode ::
          :append
          | :binary
          | :charlist
          | :compressed
          | :delayed_write
          | :exclusive
          | :raw
          | :read
          | :read_ahead
          | :sync
          | :write
          | {:read_ahead, pos_integer}
          | {:delayed_write, non_neg_integer, non_neg_integer}
          | encoding_mode()

  @type encoding_mode ::
          :utf8
          | {
              :encoding,
              :latin1
              | :unicode
              | :utf8
              | :utf16
              | :utf32
              | {:utf16, :big | :little}
              | {:utf32, :big | :little}
            }

  @type stream_mode ::
          encoding_mode()
          | :trim_bom
          | {:read_ahead, pos_integer | false}
          | {:delayed_write, non_neg_integer, non_neg_integer}

  @doc """
  Returns `true` if the path is a regular file.

  This function follows symbolic links, so if a symbolic link points to a
  regular file, `true` is returned.

  ## Options

  The supported options are:

    * `:raw` - a single atom to bypass the file server and only check
      for the file locally

  ## Examples

      File.regular?(__ENV__.file)
      #=> true

  """
  @spec regular?(Path.t(), [regular_option]) :: boolean
        when regular_option: :raw
  def regular?(path, opts \\ []) do
    :elixir_utils.read_file_type(IO.chardata_to_string(path), opts) == {:ok, :regular}
  end

  @doc """
  Returns `true` if the given path is a directory.

  This function follows symbolic links, so if a symbolic link points to a
  directory, `true` is returned.

  ## Options

  The supported options are:

    * `:raw` - a single atom to bypass the file server and only check
      for the file locally

  ## Examples

      File.dir?("./test")
      #=> true

      File.dir?("test")
      #=> true

      File.dir?("/usr/bin")
      #=> true

      File.dir?("~/Downloads")
      #=> false

      "~/Downloads" |> Path.expand() |> File.dir?()
      #=> true

  """
  @spec dir?(Path.t(), [dir_option]) :: boolean
        when dir_option: :raw
  def dir?(path, opts \\ []) do
    :elixir_utils.read_file_type(IO.chardata_to_string(path), opts) == {:ok, :directory}
  end

  @doc """
  Returns `true` if the given path exists.

  It can be a regular file, directory, socket, symbolic link, named pipe or device file.
  Returns `false` for symbolic links pointing to non-existing targets.

  ## Options

  The supported options are:

    * `:raw` - a single atom to bypass the file server and only check
      for the file locally

  ## Examples

      File.exists?("test/")
      #=> true

      File.exists?("missing.txt")
      #=> false

      File.exists?("/dev/null")
      #=> true

  """
  @spec exists?(Path.t(), [exists_option]) :: boolean
        when exists_option: :raw
  def exists?(path, opts \\ []) do
    opts = [{:time, :posix}] ++ opts
    match?({:ok, _}, :file.read_file_info(IO.chardata_to_string(path), opts))
  end

  @doc """
  Tries to create the directory `path`.

  Missing parent directories are not created.
  Returns `:ok` if successful, or `{:error, reason}` if an error occurs.

  Typical error reasons are:

    * `:eacces`  - missing search or write permissions for the parent
      directories of `path`
    * `:eexist`  - there is already a file or directory named `path`
    * `:enoent`  - a component of `path` does not exist
    * `:enospc`  - there is no space left on the device
    * `:enotdir` - a component of `path` is not a directory;
      on some platforms, `:enoent` is returned instead

  """
  @spec mkdir(Path.t()) :: :ok | {:error, posix}
  def mkdir(path) do
    :file.make_dir(IO.chardata_to_string(path))
  end

  @doc """
  Returns `{:ok, binary}`, where `binary` is a binary data object that contains the contents
  of `path`, or `{:error, reason}` if an error occurs.

  Typical error reasons:

    * `:enoent`  - the file does not exist
    * `:eacces`  - missing permission for reading the file,
      or for searching one of the parent directories
    * `:eisdir`  - the named file is a directory
    * `:enotdir` - a component of the file name is not a directory;
      on some platforms, `:enonent` is returned instead
    * `:enomem`  - there is not enough memory for the contents of the file

  You can use `:file.format_error/1` to get a descriptive string of the error.
  """
  @spec read(Path.t()) :: {:ok, binary} | {:error, posix}
  def read(path) do
    :file.read_file(IO.chardata_to_string(path))
  end

  @doc """
  Returns a binary with the content of the given filename or raises
  `File.Error` if an error occurs.
  """
  @spec read!(Path.t()) :: binary
  def read!(path) do
    case read(path) do
      {:ok, binary} ->
        binary

      {:error, reason} ->
        raise File.Error, reason: reason, action: "read file", path: IO.chardata_to_string(path)
    end
  end
end
