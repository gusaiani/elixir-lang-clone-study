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


