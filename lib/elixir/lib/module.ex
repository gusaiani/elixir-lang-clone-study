defmodule Module do
  @moduledoc ~S'''
  Provides functions to deal with modules during compilation time.

  It allows a developer to dynamically add, delete and register
  attributes, attach documentation and so forth.

  After a module is compiled, using many of the functions in
  this module will raise errors, since it is out of their scope
  to inspect runtime data. Most of the runtime data can be inspected
  via the `__info__/1` function attached to each compiled module.

  ## Module attributes

  Each module can be decorated with one or more attributes. The following ones
  are currently defined by Elixir:

  ### `@after_compile`

  A hook that will be invoked right after the current module is compiled.
  Accepts a module or a `{module, function_name}`. See the "Compile callbacks"
  section below.

  ### `@before_compile`

  A hook that will be invoked before the module is compiled.
  Accepts a module or a `{module, function_or_macro_name}` tuple.
  See the "Compile callbacks" section below.

  ### `@behaviour` (notice the British spelling)

  Behaviours can be referenced by modules to ensure they implement
  required specific function signatures defined by `@callback`.

  For example, you could specify a `URI.Parser` behaviour as follows:

      defmodule URI.Parser do
        @doc "Defines a default port"
        @callback default_port() :: integer

        @doc "Parses the given URL"
        @callback parse(uri_info :: URI.t) :: URI.t
      end

  And then a module may use it as:

      defmodule URI.HTTP do
        @behaviour URI.Parser
        def default_port(), do: 80
        def parse(info), do: info
      end

  If the behaviour changes or `URI.HTTP` does not implement
  one of the callbacks, a warning will be raised.

  ### `@impl`

  To aid in the correct implementation of behaviours, you may optionally declare
  `@impl` for implemented callbacks of a behaviour. This makes callbacks
  explicit and can help you to catch errors in your code (the compiler will warn
  you if you mark a function as `@impl` when in fact it is not a callback, and
  vice versa). It also helps with maintainability by making it clear to other
  developers that the function's purpose is to implement a callback.

  Using `@impl` the example above can be rewritten as:

      defmodule URI.HTTP do
        @behaviour URI.parser

        @impl true
        def default_port(), do: 80

        @impl true
        def parse(info), do: info
      end

  You may pass either `false`, `true`, or a specific behaviour to `@impl`.

      defmodule Foo do
        @behaviour Bar
        @behaviour Baz

        @impl true # will warn if neither Bar nor Baz specify a callback named bar/0
        def bar(), do: :ok

        @impl Baz # will warn if Baz does not specify a callback named baz/0
        def baz(), do: :ok
      end

  ### `@compile`

  Defines options for module compilation. This is used to configure
  both Elixir and Erlang compilers, as any other compilation pass
  added by external tools. For example:

      defmodule MyModule do
        @compile {:inline, my_fun: 1}

        def my_fun(arg) do
          to_string(arg)
        end
      end

  Multiple uses of `@compile` will accumulate instead of overriding
  previous ones. See the "Compile options" section below.

  ### `@doc` (and `@since`)

  Provides documentation for the function or macro that follows the
  attribute.

  Accepts a string (often a heredoc) or `false` where `@doc false` will
  make the function/macro invisible to documentation extraction tools
  like ExDoc. For example:

      defmodule MyModule do
        @doc "Hello world"
        @since "1.1.0"
        def hello do
          "world"
        end

        @doc """
        Sums `a` to `b`.
        """
        def sum(a, b) do
          a + b
        end
      end

  `@since` is an optional attribute that annotates which version the
  function was introduced.

  ### `@dialyzer`

  Defines warnings to request or suppress when using a version of
  `:dialyzer` that supports module attributes.

  Accepts an atom, a tuple, or a list of atoms and tuples. For example:

      defmodule MyModule do
        @dialyzer {:nowarn_function, my_fun: 1}

        def my_fun(arg) do
          M.not_a_function(arg)
        end
      end

  For the list of supported warnings, see
  [`:dialyzer` module](http://www.erlang.org/doc/man/dialyzer.html).

  Multiple uses of `@dialyzer` will accumulate instead of overriding
  previous ones.

  ### `@external_resource`

  Specifies an external resource for the current module.

  Sometimes a module embeds information from an external file. This
  attribute allows the module to annotate which external resources
  have been used.

  Tools like Mix may use this information to ensure the module is
  recompiled in case any of the external resources change.

  ### `@file`

  Changes the filename used in stacktraces for the function or macro that
  follows the attribute, such as:

      defmodule MyModule do
        @doc "Hello world"
        @file "hello.ex"
        def hello do
          "world"
        end
      end

  ### `@moduledoc`

  Provides documentation for the current module.

      defmodule MyModule do
        @moduledoc """
        A very useful module.
        """
      end

  Accepts a string (often a heredoc) or `false` where
  `@moduledoc false` will make the module invisible to
  documentation extraction tools like ExDoc.

  ### `@on_definition`

  A hook that will be invoked when each function or macro in the current
  module is defined. Useful when annotating functions.

  Accepts a module or a `{module, function_name}` tuple. See the
  "Compile callbacks" section below.

  ### `@on_load`

  A hook that will be invoked whenever the module is loaded.

  Accepts the function name (as an atom) of a function in the current module or
  `{function_name, 0}` tuple where `function_name` is the name of a function in
  the current module. The function must have arity 0 (no arguments) and has to
  return `:ok`, otherwise the loading of the module will be aborted. For
  example:

      defmodule MyModule do
        @on_load :load_check

        def load_check do
          if some_condition() do
            :ok
          else
            :abort
          end
        end

        def some_condition do
          false
        end
      end

  Modules compiled with HiPE would not call this hook.

  ### `@vsn`

  Specify the module version. Accepts any valid Elixir value, for example:

      defmodule MyModule do
        @vsn "1.0"
      end

  ### Typespec attributes

  The following attributes are part of typespecs and are also reserved by
  Elixir:

    * `@type` - defines a type to be used in `@spec`
    * `@typep` - defines a private type to be used in `@spec`
    * `@opaque` - defines an opaque type to be used in `@spec`
    * `@spec` - provides a specification for a function
    * `@callback` - provides a specification for a behaviour callback
    * `@macrocallback` - provides a specification for a macro behaviour callback
    * `@optional_callbacks` - specifies which behaviour callbacks and macro
      behaviour callbacks are optional
    * `@impl` - declares an implementation of a callback function or macro

  ### Custom attributes

  In addition to the built-in attributes outlined above, custom attributes may
  also be added. A custom attribute is any valid identifier prefixed with an
  `@` and followed by a valid Elixir value:

      defmodule MyModule do
        @custom_attr [some: "stuff"]
      end

  For more advanced options available when defining custom attributes, see
  `register_attribute/3`.

  ## Compile callbacks

  There are three callbacks that are invoked when functions are defined,
  as well as before and immediately after the module bytecode is generated.

  ### `@after_compile`

  A hook that will be invoked right after the current module is compiled.

  Accepts a module or a `{module, function_name}` tuple. The function
  must take two arguments: the module environment and its bytecode.
  When just a module is provided, the function is assumed to be
  `__after_compile__/2`.

  #### Example

      defmodule MyModule do
        @after_compile __MODULE__

        def __after_compile__(env, _bytecode) do
          IO.inspect env
        end
      end

  ### `@before_compile`

  A hook that will be invoked before the module is compiled.

  Accepts a module or a `{module, function_or_macro_name}` tuple. The
  function/macro must take one argument: the module environment. If it's a
  macro, its returned value will be injected at the end of the module definition
  before the compilation starts.

  When just a module is provided, the function/macro is assumed to be
  `__before_compile__/1`.

  *Note*: unlike `@after_compile`, the callback function/macro must
  be placed in a separate module (because when the callback is invoked,
  the current module does not yet exist).

  #### Example

      defmodule A do
        defmacro __before_compile__(_env) do
          quote do
            def hello, do: "world"
          end
        end
      end

      defmodule B do
        @before_compile A
      end

      B.hello()
      #=> "world"

  ### `@on_definition`

  A hook that will be invoked when each function or macro in the current
  module is defined. Useful when annotating functions.

  Accepts a module or a `{module, function_name}` tuple. The function
  must take 6 arguments:

    * the module environment
    * the kind of the function/macro: `:def`, `:defp`, `:defmacro`, or `:defmacrop`
    * the function/macro name
    * the list of quoted arguments
    * the list of quoted guards
    * the quoted function body

  Note the hook receives the quoted arguments and it is invoked before
  the function is stored in the module. So `Module.defines?/2` will return
  `false` for the first clause of every function.

  If the function/macro being defined has multiple clauses, the hook will
  be called for each clause.

  Unlike other hooks, `@on_definition` will only invoke functions and
  never macros. This is to avoid `@on_definition` callbacks from
  redefining functions that have just been defined in favor of more
  explicit approaches.

  When just a module is provided, the function is assumed to be
  `__on_definition__/6`.

  #### Example

      defmodule Hooks do
        def on_def(_env, kind, name, args, guards, body) do
          IO.puts "Defining #{kind} named #{name} with args:"
          IO.inspect args
          IO.puts "and guards"
          IO.inspect guards
          IO.puts "and body"
          IO.puts Macro.to_string(body)
        end
      end

      defmodule MyModule do
        @on_definition {Hooks, :on_def}

        def hello(arg) when is_binary(arg) or is_list(arg) do
          "Hello" <> to_string(arg)
        end

        def hello(_) do
          :ok
        end
      end

  ## Compile options

  The `@compile` attribute accepts different options that are used by both
  Elixir and Erlang compilers. Some of the common use cases are documented
  below:

    * `@compile :debug_info` - includes `:debug_info` regardless of the
      corresponding setting in `Code.compiler_options/1`

    * `@compile {:debug_info, false}` - disables `:debug_info` regardless
      of the corresponding setting in `Code.compiler_options/1`

    * `@compile {:inline, some_fun: 2, other_fun: 3}` - inlines the given
      name/arity pairs

    * `@compile {:autoload, false}` - disables automatic loading of
      modules after compilation. Instead, the module will be loaded after
      it is dispatched to

  You can see a handful more options used by the Erlang compiler in
  the documentation for the [`:compile` module](http://www.erlang.org/doc/man/compile.html).
  '''

  @typep definition :: {atom, arity}
  @typep def_kind :: :def | :defp | :defmacro | :defmacrop

  @doc """
  Provides runtime information about functions and macros defined by the
  module, etc.

  Each module gets an `__info__/1` function when it's compiled. The function
  takes one of the following atoms:

    * `:functions` - keyword list of public functions along with their arities

    * `:macros` - keyword list of public macros along with their arities

    * `:module` - the module atom name

    * `:md5` - the MD5 of the module

    * `:compile` - a list with compiler metadata

    * `:attributes` - a list with all persisted attributes

  """
  def __info__(kind)

  @doc """
  Checks if a module is open.

  A module is "open" if it is currently being defined and its attributes and
  functions can be modified.
  """
  @spec open?(module) :: boolean
  def open?(module) when is_atom(module) do
    :elixir_module.is_open(module)
  end

  @doc """
  Evaluates the quoted contents in the given module's context.

  A list of environment options can also be given as argument.
  See `Code.eval_string/3` for more information.

  Raises an error if the module was already compiled.

  ## Examples

      defmodule Foo do
        contents = quote do: (def sum(a, b), do: a + b)
        Module.eval_quoted __MODULE__, contents
      end

      Foo.sum(1, 2) #=> 3

  For convenience, you can pass any `Macro.Env` struct, such
  as  `__ENV__/0`, as the first argument or as options. Both
  the module and all options will be automatically extracted
  from the environment:

      defmodule Foo do
        contents = quote do: (def sum(a, b), do: a + b)
        Module.eval_quoted __ENV__, contents
      end

      Foo.sum(1, 2) #=> 3

  Note that if you pass a `Macro.Env` struct as first argument
  while also passing `opts`, they will be merged with `opts`
  having precedence.
  """
  @spec eval_quoted(module | Macro.Env.t(), Macro.t(), list, keyword | Macro.Env.t()) :: term
  def eval_quoted(module_or_env, quoted, binding \\ [], opts \\ [])

  def eval_quoted(%Macro.Env{} = env, quoted, binding, opts)
      when is_list(binding) and is_list(opts) do
    eval_quoted(env.module, quoted, binding, Keyword.merge(Map.to_list(env), opts))
  end

  def eval_quoted(module, quoted, binding, %Macro.Env{} = env)
      when is_atom(module) and is_list(binding) do
    eval_quoted(module, quoted, binding, Map.to_list(env))
  end

  def eval_quoted(module, quoted, binding, opts)
      when is_atom(module) and is_list(binding) and is_list(opts) do
    assert_not_compiled!(:eval_quoted, module)
    :elixir_def.reset_last(module)

    {value, binding, _env, _scope} =
      :elixir.eval_quoted(quoted, binding, Keyword.put(opts, :module, module))

    {value, binding}
  end

  @doc """
  Creates a module with the given name and defined by
  the given quoted expressions.

  The line where the module is defined and its file **must**
  be passed as options.

  It returns a tuple of shape `{:module, module, binary, term}`
  where `module` is the module name, `binary` is the module
  byte code and `term` is the result of the last expression in
  `quoted`.

  Similar to `Kernel.defmodule/2`, the binary will only be
  written to disk as a `.beam` file if `Module.create/3` is
  invoked in a file that is currently being compiled.

  ## Examples

      contents =
        quote do
          def world, do: true
        end

      Module.create(Hello, contents, Macro.Env.location(__ENV__))

      Hello.world #=> true

  ## Differences from `defmodule`

  `Module.create/3` works similarly to `Kernel.defmodule/2`
  and return the same results. While one could also use
  `defmodule` to define modules dynamically, this function
  is preferred when the module body is given by a quoted
  expression.

  Another important distinction is that `Module.create/3`
  allows you to control the environment variables used
  when defining the module, while `Kernel.defmodule/2`
  automatically uses the environment it is invoked at.
  """
  @spec create(module, Macro.t(), Macro.Env.t() | keyword) :: {:module, module, binary, term}
  def create(module, quoted, opts)

  def create(module, quoted, %Macro.Env{} = env) when is_atom(module) do
    create(module, quoted, Map.to_list(env))
  end

  def create(module, quoted, opts) when is_atom(module) and is_list(opts) do
    unless Keyword.has_key?(opts, :file) do
      raise ArgumentError, "expected :file to be given as option"
    end

    next = :erlang.unique_integer()
    line = Keyword.get(opts, :line, 0)
    quoted = :elixir_quote.linify_with_context_counter(line, {module, next}, quoted)
    :elixir_module.compile(module, quoted, [], :elixir.env_for_eval(opts))
  end

  @doc """
  Concatenates a list of aliases and returns a new alias.

  ## Examples

      iex> Module.concat([Foo, Bar])
      Foo.Bar

      iex> Module.concat([Foo, "Bar"])
      Foo.Bar

  """
  @spec concat([binary | atom]) :: atom
  def concat(list) when is_list(list) do
    :elixir_aliases.concat(list)
  end

  @doc """
  Concatenates two aliases and returns a new alias.

  ## Examples

      iex> Module.concat(Foo, Bar)
      Foo.Bar

      iex> Module.concat(Foo, "Bar")
      Foo.Bar

  """
  @spec concat(binary | atom, binary | atom) :: atom
  def concat(left, right)
      when (is_binary(left) or is_atom(left)) and (is_binary(right) or is_atom(right)) do
    :elixir_aliases.concat([left, right])
  end

  @doc """
  Concatenates a list of aliases and returns a new alias only if the alias
  was already referenced.

  If the alias was not referenced yet, fails with `ArgumentError`.
  It handles charlists, binaries and atoms.

  ## Examples

      iex> Module.safe_concat([Module, Unknown])
      ** (ArgumentError) argument error

      iex> Module.safe_concat([List, Chars])
      List.Chars

  """
  @spec safe_concat([binary | atom]) :: atom
  def safe_concat(list) when is_list(list) do
    :elixir_aliases.safe_concat(list)
  end

  @doc """
  Concatenates two aliases and returns a new alias only if the alias was
  already referenced.

  If the alias was not referenced yet, fails with `ArgumentError`.
  It handles charlists, binaries and atoms.

  ## Examples

      iex> Module.safe_concat(Module, Unknown)
      ** (ArgumentError) argument error

      iex> Module.safe_concat(List, Chars)
      List.Chars

  """
  @spec safe_concat(binary | atom, binary | atom) :: atom
  def safe_concat(left, right)
      when (is_binary(left) or is_atom(left)) and (is_binary(right) or is_atom(right)) do
    :elixir_aliases.safe_concat([left, right])
  end

  # Build signatures to be stored in docs

  defp build_signature(args, env) do
    {reverse_args, counters} = simplify_args(args, %{}, [], env)
    expand_vars(reverse_args, counters, [])
  end

  defp simplify_args([arg | args], counters, acc, env) do
    {arg, counters} = simplify_arg(arg, counters, env)
    simplify_args(args, counters, [arg | acc], env)
  end

  defp simplify_args([], counters, reverse_args, _env) do
    {reverse_args, counters}
  end

  defp simplify_arg({:\\, _, [left, right]}, acc, env) do
    {left, acc} = simplify_arg(left, acc, env)

    right =
      Macro.prewalk(right, fn
        {:@, _, _} = attr -> Macro.expand_once(attr, env)
        other -> other
      end)

    {{:\\, [], [left, right]}, acc}
  end

  # If the variable is being used explicitly for naming,
  # we always give it a higher priority (nil) even if it
  # starts with underscore.
  defp simplify_arg({:=, _, [{var, _, atom}, _]}, acc, _env) when is_atom(atom) do
    {simplify_var(var, nil), acc}
  end

  defp simplify_arg({:=, _, [_, {var, _, atom}]}, acc, _env) when is_atom(atom) do
    {simplify_var(var, nil), acc}
  end

  # If we have only the variable as argument, it also gets
  # higher priority. However, if the variable starts with an
  # underscore, we give it a secondary context (Elixir) with
  # lower priority.
  defp simplify_arg({var, _, atom}, acc, _env) when is_atom(atom) do
    {simplify_var(var, Elixir), acc}
  end

  defp simplify_arg({:%, _, [left, _]}, acc, env) do
    case Macro.expand_once(left, env) do
      module when is_atom(module) -> autogenerated(acc, simplify_module_name(module))
      _ -> autogenerated(acc, :struct)
    end
  end

  defp simplify_arg({:%{}, _, _}, acc, _env) do
    autogenerated(acc, :map)
  end

  defp simplify_arg({:@, _, _} = attr, acc, env) do
    simplify_arg(Macro.expand_once(attr, env), acc, env)
  end

  defp simplify_arg(other, acc, _env) when is_integer(other), do: autogenerated(acc, :int)
  defp simplify_arg(other, acc, _env) when is_boolean(other), do: autogenerated(acc, :bool)
  defp simplify_arg(other, acc, _env) when is_atom(other), do: autogenerated(acc, :atom)
  defp simplify_arg(other, acc, _env) when is_list(other), do: autogenerated(acc, :list)
  defp simplify_arg(other, acc, _env) when is_float(other), do: autogenerated(acc, :float)
  defp simplify_arg(other, acc, _env) when is_binary(other), do: autogenerated(acc, :binary)
  defp simplify_arg(_, acc, _env), do: autogenerated(acc, :arg)

  defp simplify_var(var, guess_priority) do
    case Atom.to_string(var) do
      "_" -> {:_, [], guess_priority}
      "_" <> rest -> {String.to_atom(rest), [], guess_priority}
      _ -> {var, [], nil}
    end
  end

  defp simplify_module_name(module) when is_atom(module) do
    try do
      split(module)
    rescue
      ArgumentError -> module
    else
      module_name -> String.to_atom(Macro.underscore(List.last(module_name)))
    end
  end

  defp autogenerated(acc, key) do
    case acc do
      %{^key => :once} -> {key, Map.put(acc, key, 2)}
      %{^key => value} -> {key, Map.put(acc, key, value + 1)}
      %{} -> {key, Map.put(acc, key, :once)}
    end
  end

  defp expand_vars([key | keys], counters, acc) when is_atom(key) do
    case counters do
      %{^key => count} when is_integer(count) and count >= 1 ->
        counters = Map.put(counters, key, count - 1)
        expand_vars(keys, counters, [{:"#{key}#{count}", [], Elixir} | acc])

      _ ->
        expand_vars(keys, counters, [{key, [], Elixir} | acc])
    end
  end

  defp expand_vars([arg | args], counters, acc) do
    expand_vars(args, counters, [arg | acc])
  end

  defp expand_vars([], _counters, acc) do
    acc
  end

  # Merge

  defp merge_signatures([h1 | t1], [h2 | t2], i) do
    [merge_signature(h1, h2, i) | merge_signatures(t1, t2, i + 1)]
  end

  defp merge_signatures([], [], _) do
    []
  end

  defp merge_signature({:\\, line, [left, right]}, newer, i) do
    {:\\, line, [merge_signature(left, newer, i), right]}
  end

  defp merge_signature(older, {:\\, _, [left, _]}, i) do
    merge_signature(older, left, i)
  end

  # The older signature, when given, always have higher precedence
  defp merge_signature({_, _, nil} = older, _newer, _), do: older
  defp merge_signature(_older, {_, _, nil} = newer, _), do: newer

  # Both are a guess, so check if they are the same guess
  defp merge_signature({var, _, _} = older, {var, _, _}, _), do: older

  # Otherwise, returns a generic guess
  defp merge_signature({_, line, _}, _newer, i), do: {:"arg#{i}", line, Elixir}
