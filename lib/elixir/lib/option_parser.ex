defmodule OptionParser do
  @moduledoc """
  This module contains functions to parse command line options.
  """

  @type argv :: [String.t()]
  @type parsed :: keyword
  @type errors :: [{String.t(), String.t() | nil}]
  @type options :: [switches: keyword, strict: keyword, aliases: keyword]

  defmodule ParseError do
    defexception [:message]
  end

  @doc """
  Parses `argv` into a keyword list.

  It returns a three-element tuple with the form `{parsed, args, invalid}`, where:

    * `parsed` is a keyword list of parsed switches with `{switch_name, value}`
      tuples in it; `switch_name` is the atom representing the switch name while
      `value` is the value for that switch parsed according to `opts` (see the
      "Examples" section for more information)
    * `args` is a list of the remaining arguments in `argv` as strings
    * `invalid` is a list of invalid options as `{option_name, value}` where
      `option_name` is the raw option and `value` is `nil` if the option wasn't
      expected or the string value if the value didn't have the expected type for
      the corresponding option

  Elixir converts switches to underscored atoms, so `--source-path` becomes
  `:source_path`. This is done to better suit Elixir conventions. However, this
  means that switches can't contain underscores and switches that do contain
  underscores are always returned in the list of invalid switches.

  When parsing, it is common to list switches and their expected types:

      iex> OptionParser.parse(["--debug"], strict: [debug: :boolean])
      {[debug: true], [], []}

      iex> OptionParser.parse(["--source", "lib"], strict: [source: :string])
      {[source: "lib"], [], []}

      iex> OptionParser.parse(["--source-path", "lib", "test/enum_test.exs", "--verbose"],
      ...>                    strict: [source_path: :string, verbose: :boolean])
      {[source_path: "lib", verbose: true], ["test/enum_test.exs"], []}

  We will explore the valid switches and operation modes of option parser below.

  ## Options

  The following options are supported:

    * `:switches` or `:strict` - see the "Switch definitions" section below
    * `:allow_nonexistent_atoms` - see the "Parsing unknown switches" section below
    * `:aliases` - see the "Aliases" section below

  ## Switch definitions

  Switches can be specified via one of two options:

    * `:strict` - defines strict switches. Any switch in `argv` that is not
      specified in the list is returned in the invalid options list.
    * `:switches` - defines some switches and their types. This function
      still attempts to parse switches that are not in this list.

  Both these options accept a keyword list of `{name, type}` tuples where `name`
  is an atom defining the name of the switch and `type` is an atom that
  specifies the type for the value of this switch (see the "Types" section below
  for the possible types and more information about type casting).

  Note that you should only supply the `:switches` or the `:strict` option.
  If you supply both, an `ArgumentError` exception will be raised.

  ### Types

  Switches parsed by `OptionParser` may take zero or one arguments.

  The following switches types take no arguments:

    * `:boolean` - sets the value to `true` when given (see also the
      "Negation switches" section below)
    * `:count` - counts the number of times the switch is given

  The following switches take one argument:

    * `:integer` - parses the value as an integer
    * `:float` - parses the value as a float
    * `:string` - parses the value as a string

  If a switch can't be parsed according to the given type, it is
  returned in the invalid options list.

  ### Modifiers

  Switches can be specified with modifiers, which change how
  they behave. The following modifiers are supported:

    * `:keep` - keeps duplicated items instead of overriding them;
      works with all types except `:count`. Specifying `switch_name: :keep`
      assumes the type of `:switch_name` will be `:string`.

  To use `:keep` with a type other than `:string`, use a list as the type
  for the switch. For example: `[foo: [:integer, :keep]]`.

  ### Negation switches

  In case a switch `SWITCH` is specified to have type `:boolean`, it may be
  passed as `--no-SWITCH` as well which will set the option to `false`:

      iex> OptionParser.parse(["--no-op", "path/to/file"], switches: [op: :boolean])
      {[op: false], ["path/to/file"], []}

  ### Parsing unknown switches

  When the `:switches` option is given, `OptionParser` will attempt to parse
  unknown switches:

      iex> OptionParser.parse(["--debug"], switches: [key: :string])
      {[debug: true], [], []}

  Even though we haven't specified `--debug` in the list of switches, it is part
  of the returned options. This would also work:

      iex> OptionParser.parse(["--debug", "value"], switches: [key: :string])
      {[debug: "value"], [], []}

  Switches followed by a value will be assigned the value, as a string. Switches
  without an argument will be set automatically to `true`. Since we cannot assert
  the type of the switch value, it is preferred to use the `:strict` option that
  accepts only known switches and always verify their types.

  If you do want to parse unknown switches, remember that Elixir converts switches
  to atoms. Since atoms are not garbage collected, OptionParser will only parse
  switches that translate to atoms used by the runtime to avoid leaking atoms.
  For instance, the code below will discard the `--option-parser-example` switch
  because the `:option_parser_example` atom is never used anywhere:

      OptionParser.parse(["--option-parser-example"], switches: [debug: :boolean])
      # The :option_parser_example atom is not used anywhere below

  However, the code below would work as long as `:option_parser_example` atom is
  used at some point later (or earlier) **in the same module**:

      {opts, _, _} = OptionParser.parse(["--option-parser-example"], switches: [debug: :boolean])
      opts[:option_parser_example]

  In other words, Elixir will do the correct thing and only parse options that are
  used by the runtime, ignoring all others. If you would like to parse all switches,
  regardless if they exist or not, you can force creation of atoms by passing
  `allow_nonexistent_atoms: true` as option. Use this option with care. It is only
  useful when you are building command-line applications that receive
  dynamically-named arguments and must be avoided in long-running systems.

  ## Aliases

  A set of aliases can be specified in the `:aliases` option:

      iex> OptionParser.parse(["-d"], aliases: [d: :debug], strict: [debug: :boolean])
      {[debug: true], [], []}

  ## Examples

  Here are some examples of working with different types and modifiers:

      iex> OptionParser.parse(["--unlock", "path/to/file"], strict: [unlock: :boolean])
      {[unlock: true], ["path/to/file"], []}

      iex> OptionParser.parse(["--unlock", "--limit", "0", "path/to/file"],
      ...>                    strict: [unlock: :boolean, limit: :integer])
      {[unlock: true, limit: 0], ["path/to/file"], []}

      iex> OptionParser.parse(["--limit", "3"], strict: [limit: :integer])
      {[limit: 3], [], []}

      iex> OptionParser.parse(["--limit", "xyz"], strict: [limit: :integer])
      {[], [], [{"--limit", "xyz"}]}

      iex> OptionParser.parse(["--verbose"], switches: [verbose: :count])
      {[verbose: 1], [], []}

      iex> OptionParser.parse(["-v", "-v"], aliases: [v: :verbose], strict: [verbose: :count])
      {[verbose: 2], [], []}

      iex> OptionParser.parse(["--unknown", "xyz"], strict: [])
      {[], ["xyz"], [{"--unknown", nil}]}

      iex> OptionParser.parse(["--limit", "3", "--unknown", "xyz"],
      ...>                    switches: [limit: :integer])
      {[limit: 3, unknown: "xyz"], [], []}

      iex> OptionParser.parse(["--unlock", "path/to/file", "--unlock", "path/to/another/file"], strict: [unlock: :keep])
      {[unlock: "path/to/file", unlock: "path/to/another/file"], [], []}

  """
  @spec parse(argv, options) :: {parsed, argv, errors}
  def parse(argv, opts \\ []) when is_list(argv) and is_list(opts) do
    do_parse(argv, build_config(opts), [], [], [], true)
  end

  @doc """
  The same as `parse/2` but raises an `OptionParser.ParseError`
  exception if any invalid options are given.

  If there are no errors, returns a `{parsed, rest}` tuple where:

    * `parsed` is the list of parsed switches (same as in `parse/2`)
    * `rest` is the list of arguments (same as in `parse/2`)

  ## Examples

      iex> OptionParser.parse!(["--debug", "path/to/file"], strict: [debug: :boolean])
      {[debug: true], ["path/to/file"]}

      iex> OptionParser.parse!(["--limit", "xyz"], strict: [limit: :integer])
      ** (OptionParser.ParseError) 1 error found!
      --limit : Expected type integer, got "xyz"

      iex> OptionParser.parse!(["--unknown", "xyz"], strict: [])
      ** (OptionParser.ParseError) 1 error found!
      --unknown : Unknown option

      iex> OptionParser.parse!(["-l", "xyz", "-f", "bar"],
      ...>                     switches: [limit: :integer, foo: :integer], aliases: [l: :limit, f: :foo])
      ** (OptionParser.ParseError) 2 errors found!
      -l : Expected type integer, got "xyz"
      -f : Expected type integer, got "bar"

  """
  @spec parse!(argv, options) :: {parsed, argv} | no_return
  def parse!(argv, opts \\ []) when is_list(argv) and is_list(opts) do
    case parse(argv, opts) do
      {parsed, args, []} -> {parsed, args}
      {_, _, errors} -> raise ParseError, format_errors(errors, opts)
    end
  end

  @doc """
  Similar to `parse/2` but only parses the head of `argv`;
  as soon as it finds a non-switch, it stops parsing.

  See `parse/2` for more information.

  ## Example

      iex> OptionParser.parse_head(["--source", "lib", "test/enum_test.exs", "--verbose"],
      ...>                         switches: [source: :string, verbose: :boolean])
      {[source: "lib"], ["test/enum_test.exs", "--verbose"], []}

      iex> OptionParser.parse_head(["--verbose", "--source", "lib", "test/enum_test.exs", "--unlock"],
      ...>                         switches: [source: :string, verbose: :boolean, unlock: :boolean])
      {[verbose: true, source: "lib"], ["test/enum_test.exs", "--unlock"], []}

  """
  @spec parse_head(argv, options) :: {parsed, argv, errors}
  def parse_head(argv, opts \\ []) when is_list(argv) and is_list(opts) do
    do_parse(argv, build_config(opts), [], [], [], false)
  end

  @doc """
  The same as `parse_head/2` but raises an `OptionParser.ParseError`
  exception if any invalid options are given.

  If there are no errors, returns a `{parsed, rest}` tuple where:

    * `parsed` is the list of parsed switches (same as in `parse_head/2`)
    * `rest` is the list of arguments (same as in `parse_head/2`)

  ## Examples

      iex> OptionParser.parse_head!(["--source", "lib", "path/to/file", "--verbose"],
      ...>                         switches: [source: :string, verbose: :boolean])
      {[source: "lib"], ["path/to/file", "--verbose"]}

      iex> OptionParser.parse_head!(["--number", "lib", "test/enum_test.exs", "--verbose"],
      ...>                          strict: [number: :integer])
      ** (OptionParser.ParseError) 1 error found!
      --number : Expected type integer, got "lib"

      iex> OptionParser.parse_head!(["--verbose", "--source", "lib", "test/enum_test.exs", "--unlock"],
      ...>                          strict: [verbose: :integer, source: :integer])
      ** (OptionParser.ParseError) 2 errors found!
      --verbose : Missing argument of type integer
      --source : Expected type integer, got "lib"

  """
  @spec parse_head!(argv, options) :: {parsed, argv} | no_return
  def parse_head!(argv, opts \\ []) when is_list(argv) and is_list(opts) do
    case parse_head(argv, opts) do
      {parsed, args, []} -> {parsed, args}
      {_, _, errors} -> raise ParseError, format_errors(errors, opts)
    end
  end

  defp do_parse([], _config, opts, args, invalid, _all?) do
    {Enum.reverse(opts), Enum.reverse(args), Enum.reverse(invalid)}
  end

  defp do_parse(argv, %{switches: switches} = config, opts, args, invalid, all?) do
    case next_with_config(argv, config) do
      {:ok, option, value, rest} ->
        # the option exists and it was successfully parsed
        kinds = List.wrap(Keyword.get(switches, option))
        new_opts = store_option(opts, option, value, kinds)
        do_parse(rest, config, new_opts, args, invalid, all?)

      {:invalid, option, value, rest} ->
        # the option exist but it has wrong value
        do_parse(rest, config, opts, args, [{option, value} | invalid], all?)

      {:undefined, option, _value, rest} ->
        invalid = if config.strict?, do: [{option, nil} | invalid], else: invalid
        do_parse(rest, config, opts, args, invalid, all?)

      {:error, ["--" | rest]} ->
        {Enum.reverse(opts), Enum.reverse(args, rest), Enum.reverse(invalid)}

      {:error, [arg | rest] = remaining_args} ->
        # there is no option
        if all? do
          do_parse(rest, config, opts, [arg | args], invalid, all?)
        else
          {Enum.reverse(opts), Enum.reverse(args, remaining_args), Enum.reverse(invalid)}
        end
    end
  end

  @doc """
  Low-level function that parses one option.

  It accepts the same options as `parse/2` and `parse_head/2`
  as both functions are built on top of this function. This function
  may return:

    * `{:ok, key, value, rest}` - the option `key` with `value` was
      successfully parsed

    * `{:invalid, key, value, rest}` - the option `key` is invalid with `value`
      (returned when the value cannot be parsed according to the switch type)

    * `{:undefined, key, value, rest}` - the option `key` is undefined
      (returned in strict mode when the switch is unknown or on nonexistent atoms)

    * `{:error, rest}` - there are no switches at the head of the given `argv`

  """
  @spec next(argv, options) ::
          {:ok, key :: atom, value :: term, argv}
          | {:invalid, String.t(), String.t() | nil, argv}
          | {:undefined, String.t(), String.t() | nil, argv}
          | {:error, argv}

  def next(argv, opts \\ []) when is_list(argv) and is_list(opts) do
    next_with_config(argv, build_config(opts))
  end

  defp next_with_config([], _config) do
    {:error, []}
  end

  defp next_with_config(["--" | _] = argv, _config) do
    {:error, argv}
  end

  defp next_with_config(["-" | _] = argv, _config) do
    {:error, argv}
  end

  defp next_with_config(["- " <> _ | _] = argv, _config) do
    {:error, argv}
  end

  # Handles --foo or --foo=bar
  defp next_with_config(["--" <> option | rest], config) do
    {option, value} = split_option(option)

    if String.contains?(option, ["_"]) do
      {:undefined, "--" <> option, value, rest}
    else
      tagged = tag_option(option, config)
      next_tagged(tagged, value, "--" <> option, rest, config)
    end
  end

  # Handles -a, -abc, -abc=something
  defp next_with_config(["-" <> option | rest] = argv, config) do
    %{allow_nonexistent_atoms?: allow_nonexistent_atoms?} = config
    {option, value} = split_option(option)
    original = "-" <> option
    letters = String.graphemes(option)

    cond do
      is_nil(value) and negative_number?(original) ->
        {:error, argv}

      String.contains?(option, ["-", "_"]) ->
        {:undefined, original, value, rest}

      tl(letters) == [] ->
        # We have a regular one-letter alias here
        tagged = tag_oneletter_alias(option, config)
        next_tagged(tagged, value, original, rest, config)

      true ->
        key = get_option_key(option, allow_nonexistent_atoms?)
        option_key = config.aliases[key]

        if key && option_key do
          # TODO: Remove this in Elixir v2.0
          IO.warn("multi-letter aliases are deprecated, got: #{inspect(key)}")
          next_tagged({:default, option_key}, value, original, rest, config)
        else
          next_with_config(expand_multiletter_alias(letters, value) ++ rest, config)
        end
    end
  end

  defp next_with_config(argv, _config) do
    {:error, argv}
  end

  defp next_tagged(:unknown, value, original, rest, _) do
    {value, _kinds, rest} = normalize_value(value, [], rest)
    {:undefined, original, value, rest}
  end

  defp next_tagged({tag, option}, value, original, rest, %{switches: switches, strict?: strict?}) do
    if strict? and not Keyword.has_key?(switches, option) do
      {:undefined, original, value, rest}
    else
      {kinds, value} = normalize_tag(tag, option, value, switches)
      {value, kinds, rest} = normalize_value(value, kinds, rest)

      case validate_option(value, kinds) do
        {:ok, new_value} -> {:ok, option, new_value, rest}
        :invalid -> {:invalid, original, value, rest}
      end
    end
  end

  ## Helpers

  defp build_config(opts) do
    {switches, strict?} =
      cond do
        opts[:switches] && opts[:strict] ->
          raise ArgumentError, ":switches and :strict cannot be given together"

        switches = opts[:switches] ->
          {switches, false}

        strict = opts[:strict] ->
          {strict, true}

        true ->
          #TODO: Remove this in Elixir v2.0
          IO.warn("Not passing the :switches or :strict option to OptionParser is deprecated")
          {[], false}
      end

    %{
      aliases: opts[:aliases] || [],
      allow_nonexistent_atoms? : opts[:allow_nonexistent_atoms] || false,
      strict?: strict?,
      switches: switches
    }
  end

  defp tag_option("no-" <> option = original, config) do
    %{switches: switches, allow_nonexistent_atoms?: allow_nonexistent_atoms?} = config

    cond do
      (negated = get_option_key(option, allow_nonexistent_atoms?)) &&
          :boolean in List.wrap(switches[negated]) ->
        {:negated, negated}

      option_key = get_option_key(original, allow_nonexistent_atoms?) ->
        {:default, option_key}

      true ->
        :unknown
    end
  end

  defp tag_oneletter_alias(alias, config) when is_binary(alias) do
    %{aliases: aliases, allow_nonexistent_atoms?: allow_nonexistent_atoms?} = config

    if option_key = aliases[to_existing_key(alias, allow_nonexistent_atoms?)] do
      {:default, option_key}
    else
      :unknown
    end
  end

  defp split_option(option) do
    case :binary.split(option, "=") do
      [h] -> {h, nil}
      [h, t] -> {h, t}
    end
  end

  defp format_errors([_ | _] = errors, opts) do
    types = opts[:switches] || opts[:strict]
    error_count = length(errors)
    error = if error_count == 1, do: "error", else: "errors"

    "#{error_count} #{error} found!\n" <>
      Enum.map_join(errors, "\n", &format_error(&1, opts, types))
  end

  defp format_error({option, nil}, opts, types) do
    if type = get_type(option, opts, types) do
      "#{option} : Missing argument of type #{type}"
    else
      "#{option} : Unknown option"
    end
  end

  defp format_error({option, value}, opts, types) do
    type = get_type(option, opts, types)
    "#{option} : Expected type #{type}, got #{inspect(value)}"
  end

