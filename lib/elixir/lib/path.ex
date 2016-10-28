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
      :relative -> absname_join(relative_to, path)
      :absolute -> absname_join([path])
      :volumerelative ->
        relative_to = IO.chardata_to_string(relative_to)
        absname_vr(split(path), split(relative_to), relative_to)
    end
  end

  # Joins a list
  defp adsname_join([name1, name2 | rest]), do:
    adsname_join([absname_join(name1, name2) | rest])
  defp absname_join([name]), do:
    do_absname_join(IO.chardata_to_string(name), <<>>, [], major_os_type())

  # Joins two paths
  defp absname_join(left, right),
    do: do_absname_join(IO.chardata_to_string(left), relative(right), [], major_os_type())

  defp do_absname_join(<<uc_letter, ?:, rest::binary>>, relativename, [], :win32) when uc_letter in ?A..?Z, do:
    do_absname_join(rest, relativename, [?:, uc_letter+?a-?A], :win32)
  defp do_absname_join(<<?\\, rest::binary>>, relativename, result, :win32), do:
    do_absname_join(<<?/, rest::binary>>, relativename, result, :win32)
  defp do_absname_join(<<?/, rest::binary>>, relativename, [?., ?/ | result], os_type), do:
    do_absname_join(rest, relativename, [?/ | result], os_type)
  defp do_absname_join(<<?/, rest::binary>>, [?/ | result], os_type), do:
    do_absname_join(rest, relativename, [?/ | result], os_type)
  defp do_absname_join(<<>>, <<>>, result, os_type), do:
    IO.iodata_to_binary(reverse_maybe_remove_dir_sep(result, os_type))
  defp do_absname_join(<<>>, relativename, [?: | rest], :win32), do:
    do_absname_join(relativename, <<>>, [?: | rest], :win32)


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
  defp win32_pathtype(<<c, relative::binary>>) when c in @slash, do:
    {:volumerelative, relative}
  defp win32_pathtype(<<_letter, ?:, c, relative::binary) when c in @slash, do:
    {:absolute, relative}
  defp win32_pathtype(<<_letter, ?:, relative::binary), do:
    {:volumerelative, relative}

  defp win32_pathtype([c1, c2 | relative]) when c1 in @slash and c2 in @slash, do:
    {:absolute, relative}
  defp win32_pathtype([c | relative]) when c in @slash, do:
    {:volumerelative, relative}
  defp win32_pathtype([c1, c2, list | rest]) when is_list(list), do:
    win32_pathtype([c1, c2 | list++rest])
  defp win32_pathtype([_letter, ?:, c | relative]) when c in @slash, do:
    {:absolute, relative}
  defp win32_pathtype([_letter, ?: | relative]), do:
    {:volumerelative, relative}
  defp win32_pathtype(relative), do:
    {:relative, relative}

  defp major_os_type do
    :os.type |> elem(0)
  end
end
