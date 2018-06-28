defmodule Regex do
  @moduledoc ~S"""
  Provides regular expressions for Elixir.

  Regex is based on PCRE (Perl Compatible Regular Expressions) and
  built on top of Erlang's `:re` module. More information can be found
  in the [`:re` module documentation](http://www.erlang.org/doc/man/re.html).

  Regular expressions in Elixir can be created using the sigils
  [`~r`](`Kernel.sigil_r/2`) or [`~R`](`Kernel.sigil_R/2`):

      # A simple regular expression that matches foo anywhere in the string
      ~r/foo/

      # A regular expression with case insensitive and Unicode options
      ~r/foo/iu

  Regular expressions created via sigils are pre-compiled and stored
  in the `.beam` file. Notice this may be a problem if you are precompiling
  Elixir, see the "Precompilation" section for more information.

  A Regex is represented internally as the `Regex` struct. Therefore,
  `%Regex{}` can be used whenever there is a need to match on them.
  Keep in mind it is not guaranteed two regular expressions from the
  same source are equal, for example:

      ~r/(?<foo>.)(?<bar>.)/ == ~r/(?<foo>.)(?<bar>.)/

  may return `true` or `false` depending on your machine, endianness,
  available optimizations and others. You can, however, retrieve the source
  of a compiled regular expression by accessing the `source` field, and then
  compare those directly:

      ~r/(?<foo>.)(?<bar>.)/.source == ~r/(?<foo>.)(?<bar>.)/.source

  ## Precompilation

  Regular expressions built with sigil are precompiled and stored in `.beam`
  files. This may be a problem if you are precompiling Elixir to run in
  different OTP releases, as OTP releases may update the underlying regular
  expression engine at any time.

  For such reasons, we always recommend precompiling Elixir projects using
  the Erlang/OTP version meant to run in production. In case cross-compilation is
  really necessary, you can manually invoke `Regex.recompile/1` or
  `Regex.recompile!/1` to perform a runtime version check and recompile the
  regex if necessary.

  ## Modifiers

  The modifiers available when creating a Regex are:

    * `unicode` (u) - enables Unicode specific patterns like `\p` and change
      modifiers like `\w`, `\W`, `\s` and friends to also match on Unicode.
      It expects valid Unicode strings to be given on match

    * `caseless` (i) - adds case insensitivity

    * `dotall` (s) - causes dot to match newlines and also set newline to
      anycrlf; the new line setting can be overridden by setting `(*CR)` or
      `(*LF)` or `(*CRLF)` or `(*ANY)` according to re documentation

    * `multiline` (m) - causes `^` and `$` to mark the beginning and end of
      each line; use `\A` and `\z` to match the end or beginning of the string

    * `extended` (x) - whitespace characters are ignored except when escaped
      and allow `#` to delimit comments

    * `firstline` (f) - forces the unanchored pattern to match before or at the
      first newline, though the matched text may continue over the newline

    * `ungreedy` (U) - inverts the "greediness" of the regexp
      (the previous `r` option is deprecated in favor of `U`)

  The options not available are:

    * `anchored` - not available, use `^` or `\A` instead
    * `dollar_endonly` - not available, use `\z` instead
    * `no_auto_capture` - not available, use `?:` instead
    * `newline` - not available, use `(*CR)` or `(*LF)` or `(*CRLF)` or
      `(*ANYCRLF)` or `(*ANY)` at the beginning of the regexp according to the
      re documentation

  ## Captures

  Many functions in this module handle what to capture in a regex
  match via the `:capture` option. The supported values are:

    * `:all` - all captured subpatterns including the complete matching string
      (this is the default)

    * `:first` - only the first captured subpattern, which is always the
      complete matching part of the string; all explicitly captured subpatterns
      are discarded

    * `:all_but_first` - all but the first matching subpattern, i.e. all
      explicitly captured subpatterns, but not the complete matching part of
      the string

    * `:none` - does not return matching subpatterns at all

    * `:all_names` - captures all names in the Regex

    * `list(binary)` - a list of named captures to capture

  """

  defstruct re_pattern: nil, source: "", opts: "", re_version: ""

  @type t :: %__MODULE__{re_pattern: term, source: binary, opts: binary}

  defmodule CompileError do
    defexception message: "regex could not be compiled"
  end

  @doc """
  Compiles the regular expression.

  The given options can either be a binary with the characters
  representing the same regex options given to the
  [`~r`](`Kernel.sigil_r/2`) sigil, or a list of options, as
  expected by the Erlang's `:re` module.

  It returns `{:ok, regex}` in case of success,
  `{:error, reason}` otherwise.

  ## Examples

      iex> Regex.compile("foo")
      {:ok, ~r/foo/}

      iex> Regex.compile("*foo")
      {:error, {'nothing to repeat', 0}}

  """
  @spec compile(binary, binary | [term]) :: {:ok, t} | {:error, any}
  def compile(source, options \\ "") do
    compile(source, options, version())
  end

  defp compile(source, options, version) when is_binary(options) do
    case translate_options(options, []) do
      {:error, rest} ->
        {:error, {:invalid_option, rest}}

      translated_options ->
        compile(source, translated_options, options, version)
    end
  end

  @doc """
  Returns the version of the underlying Regex engine.
  """
  @since "1.4.0"
  @spec version :: term()
  # TODO: No longer check for function_exported? on OTP 20+.
  def version do
    if function_exported?(:re, :version, 0) do
      {:re.version(), :erlang.system_info(:endian)}
    else
      {"8.33 2013-05-29", :erlang.system_info(:endian)}
    end
  end

  # Private Helpers

  defp translate_options(<<?u, t::binary>>, acc), do: translate_options(t, [:unicode, :ucp | acc])
  defp translate_options(<<?i, t::binary>>, acc), do: translate_options(t, [:caseless | acc])
  defp translate_options(<<?x, t::binary>>, acc), do: translate_options(t, [:extended | acc])
  defp translate_options(<<?f, t::binary>>, acc), do: translate_options(t, [:firstline | acc])
  defp translate_options(<<?U, t::binary>>, acc), do: translate_options(t, [:ungreedy | acc])

  defp translate_options(<<?s, t::binary>>, acc),
    do: translate_options(t, [:dotall, {:newline, :anycrlf} | acc])

  defp translate_options(<<?m, t::binary>>, acc), do: translate_options(t, [:multiline | acc])

  # TODO: Remove on 2.0
  defp translate_options(<<?r, t::binary>>, acc) do
    IO.warn("the /r modifier in regular expressions is deprecated, please use /U instead")
    translate_options(t, [:ungreedy | acc])
  end

  defp translate_options(<<>>, acc), do: acc
  defp translate_options(rest, _acc), do: {:error, rest}
end
