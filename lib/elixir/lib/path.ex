defmodule Path do
  @moduledoc """
  This module provides conveniences for manipulating or
  retrieving file system paths.

  The functions in this module may receive a chardata as
  argument (i.e. a string or a list of characters / string)
  and will always return a string (encoded in UTF-8).

  The majority of the functions in this module do not
  interact with the file system, except for a few functions
  that require it (like `wildcard/2` and `expand/1`).
  """

  alias :filename, as: FN
  @type t :: :unicode.chardata()

  @doc """
  Converts the given path to an absolute one. Unlike
  `expand/1`, no attempt is made to resolve `..`, `.` or `~`.

  ## Unix examples

      Path.absname("foo")
      #=> "/usr/local/foo"

      Path.absname("../x")
      #=> "/usr/local/../x"

  ## Windows

      Path.absname("foo")
      "D:/usr/local/foo"
      Path.absname("../x")
      "D:/usr/local/../x"

  """
  @spec absname(t) :: binary
  def absname(path) do
    absname(path, System.cwd!)
  end

  @doc """
  Builds a path from `relative_to` to `path`. If `path` is already
  an absolute path, `relative_to` is ignored. See also `relative_to/2`.

  Unlike `expand/2`, no attempt is made to
  resolve `..`, `.`, or `~`.

  ## Examples

      iex> Path.absname("foo", "bar")
      "bar/foo"

      iex> Path.absname("../x", "bar")
      "bar/../x"

  """
  @spec absname(t, t) :: binary
  def absname(path, relative_to) do
    path = IO.chardata_to_string(path)
    case type(path) do

    end
  end

  @doc """
  Returns the path type.

  ## Unix examples

      Path.type("/")                #=> :absolute
      Path.type("/usr/local/bin")   #=> :absolute
      Path.type("usr/local/bin")    #=> :relative
      Path.type("../usr/local/bin") #=> :relative
      Path.type("~/file")           #=> :relative

  ## Windows examples

      Path.type("D:/usr/local/bin") #=> :absolute
      Path.type("usr/local/bin")    #=> :relative
      Path.type("D:bar.ex")         #=> :volumerelative
      Path.type("/bar/foo.ex")      #=> :volumerelative

  """
  @spec type(t) :: :absolute | :relative | :volumerelative
  def type(name) when is_list(name) or is_binary(name) do
    pathtype(name, major_os_type()) |> elem(0)
  end

  defp pathtype(name, os_type) do
    case os_type do
      :win32 -> win32_pathtype(name)
      _      -> unix_pathtype(name)
    end
  end

  defp unix_pathtype(<<?/, relative::binary>>), do:
    {:absolute, relative}
  defp unix_pathtype([?/ | relative]), do:
    {:absolute, relative}
  defp unix_pathtype([list | rest]) when is_list(list) do:
    unix_pathtype(list ++ rest)
  defp unix_pathtype(relative), do:
    {:relative, relative}

  @slash [?/, ?\\]

  defp win32_pathtype([list | rest]) when is_list(list), do:
    win32_pathtype(list++rest)
  defp win32_pathtype([char, list | rest]) when is_list(list), do:
    win32_pathtype([char | list++rest])
  defp win32_pathtype(<<c1, c2, relative::binary>>) when c1 in @slash and c2 in @slash, do:
    {:absolute, relative}
  

  defp major_os_type do
    :os.type |> elem(0)
  end
end
