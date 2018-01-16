import Kernel, except: [to_string: 1]

defmodule Macro do
  @moduledoc ~S"""
  Conveniences for working with macros.

  ## Custom Sigils

  To create a custom sigil, define a function with the name
  `sigil_{identifier}` that takes two arguments. The first argument will be
  the string, the second will be a charlist containing any modifiers. If the
  sigil is lower case (such as `sigil_x`) then the string argument will allow
  interpolation. If the sigil is upper case (such as `sigil_X`) then the string
  will not be interpolated.

  Valid modifiers include only lower and upper case letters. Other characters
  will cause a syntax error.

  The module containing the custom sigil must be imported before the sigil
  syntax can be used.

  ### Examples

      defmodule MySigils do
        defmacro sigil_x(term, [?r]) do
          quote do
            unquote(term) |> String.reverse()
          end
        end
        defmacro sigil_x(term, _modifiers) do
          term
        end
        defmacro sigil_X(term, [?r]) do
          quote do
            unquote(term) |> String.reverse()
          end
        end
        defmacro sigil_X(term, _modifiers) do
          term
        end
      end

      import MySigils

      ~x(with #{"inter" <> "polation"})
      #=>"with interpolation"

      ~x(with #{"inter" <> "polation"})r
      #=>"noitalopretni htiw"

      ~X(without #{"interpolation"})
      #=>"without \#{"interpolation"}"

      ~X(without #{"interpolation"})r
      #=>"}\"noitalopretni\"{# tuohtiw"

  """

  alias Code.Identifier

  @typedoc "Abstract Syntax Tree (AST)"
  @type t :: expr | literal

  @typedoc "Represents expressions in the AST"
  @type expr :: {expr | atom, keyword, atom | [t]}

  @typedoc "Represents literals in the AST"
  @type literal :: atom | number | binary | fun | {t, t} | [t]

  @doc """
  Breaks a pipeline expression into a list.

  The AST for a pipeline (a sequence of applications of `|>`) is similar to the
  AST of a sequence of binary operators or function applications: the top-level
  expression is the right-most `:|>` (which is the last one to be executed), and
  its left-hand and right-hand sides are its arguments:

      quote do: 100 |> div(5) |> div(2)
      #=> {:|>, _, [arg1, arg2]}

  In the example above, the `|>` pipe is the right-most pipe; `arg1` is the AST
  for `100 |> div(5)`, and `arg2` is the AST for `div(2)`.

  It's often useful to have the AST for such a pipeline as a list of function
  applications. This function does exactly that:

      Macro.unpipe(quote do: 100 |> div(5) |> div(2))
      #=> [{100, 0}, {{:div, [], [5]}, 0}, {{:div, [], [2]}, 0}]

  We get a list that follows the pipeline directly: first the `100`, then the
  `div(5)` (more precisely, its AST), then `div(2)`. The `0` as the second
  element of the tuples is the position of the previous element in the pipeline
  inside the current function application: `{{:div, [], [5]}, 0}` means that the
  previous element (`100`) will be inserted as the 0th (first) argument to the
  `div/2` function, so that the AST for that function will become `{:div, [],
  [100, 5]}` (`div(100, 5)`).
  """
  @spec unpipe(Macro.t()) :: [Macro.t()]
  def unpipe(expr) do
    :lists.reverse(unpipe(expr, []))
  end

  defp unpipe({:|>, _, [left, right]}, acc) do
    unpipe(right, unpipe(left, acc))
  end

  defp unpipe(other, acc) do
    [{other, 0} | acc]
  end

  @doc """
  Pipes `expr` into the `call_args` at the given `position`.
  """
  @spec pipe(Macro.t(), Macro.t(), integer) :: Macro.t() | no_return
  def pipe(expr, call_args, position)

  def pipe(expr, {:&, _, _} = call_args, _integer) do
    raise ArgumentError, bad_pipe(expr, call_args)
  end

  def pipe(expr, {tuple_or_map, _, _} = call_args, _integer) when tuple_or_map in [:{}, :%{}] do
    raise ArgumentError, bad_pipe(expr, call_args)
  end

  # Without this, `Macro |> Env == Macro.Env`.
  def pipe(expr, {:__aliases__, _, _} = call_args, _integer) do
    raise ArgumentError, bad_pipe(expr, call_args)
  end

  # {:fn, _, _} is what we get when we pipe into an anonymous function without
  # calling it, e.g., `:foo |> (fn x -> x end)`.
  def pipe(expr, {:fn, _, _}, _integer) do
    expr_str = to_string(expr)

    raise ArgumentError,
          "cannot pipe #{expr_str} into an anonymous function without" <>
            " calling the function; use something like (fn ... end).() or" <>
            " define the anonymous function as a regular private function"
  end

  def pipe(expr, {call, line, atom}, integer) when is_atom(atom) do
    {call, line, List.insert_at([], integer, expr)}
  end

  def pipe(expr, {call, line, args} = call_args, integer) when is_list(args) do
    if is_atom(call) and Identifier.binary_op(call) != :error do
      raise ArgumentError,
            "cannot pipe #{to_string(expr)} into #{to_string(call_args)}, " <>
              "the #{to_string(call)} operator can only take two arguments"
    else
      {call, line, List.insert_at(args, integer, expr)}
    end
  end

  def pipe(expr, call_args, _integer) do
    raise ArgumentError, bad_pipe(expr, call_args)
  end

  defp bad_pipe(expr, call_args) do
    "cannot pipe #{to_string(expr)} into #{to_string(call_args)}, " <>
      "can only pipe into local calls foo(), remote calls Foo.bar() or anonymous functions calls foo.()"
  end

  @doc """
  Applies the given function to the node metadata if it contains one.

  This is often useful when used with `Macro.prewalk/2` to remove
  information like lines and hygienic counters from the expression
  for either storage or comparison.

  ## Examples

      iex> quoted = quote line: 10, do: sample()
      {:sample, [line: 10], []}
      iex> Macro.update_meta(quoted, &Keyword.delete(&1, :line))
      {:sample, [], []}

  """
  @spec update_meta(t, (keyword -> keyword)) :: t
  def update_meta(quoted, fun)

  def update_meta({left, meta, right}, fun) when is_list(meta) do
    {left, fun.(meta), right}
  end

  def update_meta(other, _fun) do
    other
  end

  @doc """
  Generates AST nodes for a given number of required argument variables using
  `Macro.var/2`.

  ## Examples

      iex> Macro.generate_arguments(2, __MODULE__)
      [{:var1, [], __MODULE__}, {:var2, [], __MODULE__}]

  """
  def generate_arguments(0, _), do: []

  def generate_arguments(amount, context)
      when is_integer(amount) and amount > 0 and is_atom(context) do
    for id <- 1..amount, do: Macro.var(String.to_atom("var" <> Integer.to_string(id)), context)
  end

  @doc """
  Generates an AST node representing the variable given
  by the atoms `var` and `context`.

  ## Examples

  In order to build a variable, a context is expected.
  Most of the times, in order to preserve hygiene, the
  context must be `__MODULE__/0`:

      iex> Macro.var(:foo, __MODULE__)
      {:foo, [], __MODULE__}

  However, if there is a need to access the user variable,
  nil can be given:

      iex> Macro.var(:foo, nil)
      {:foo, [], nil}

  """
  @spec var(var, context) :: {var, [], context} when var: atom, context: atom
  def var(var, context) when is_atom(var) and is_atom(context) do
    {var, [], context}
  end

  @doc """
  Performs a depth-first traversal of quoted expressions
  using an accumulator.
  """
  @spec traverse(t, any, (t, any -> {t, any}), (t, any -> {t, any})) :: {t, any}
  def traverse(ast, acc, pre, post) when is_function(pre, 2) and is_function(post, 2) do
    {ast, acc} = pre.(ast, acc)
    do_traverse(ast, acc, pre, post)
  end

  defp do_traverse({form, meta, args}, acc, pre, post) when is_atom(form) do
    {args, acc} = do_traverse_args(args, acc, pre, post)
    post.({form, meta, args}, acc)
  end

  defp do_traverse({form, meta, args}, acc, pre, post) do
    {form, acc} = pre.(form, acc)
    {form, acc} = do_traverse(form, acc, pre, post)
    {args, acc} = do_traverse_args(args, acc, pre, post)
    post.({form, meta, args}, acc)
  end

  defp do_traverse({left, right}, acc, pre, post) do
    {left, acc} = pre.(left, acc)
    {left, acc} = do_traverse(left, acc, pre, post)
    {right, acc} = pre.(right, acc)
    {right, acc} = do_traverse(right, acc, pre, post)
    post.({left, right}, acc)
  end

  defp do_traverse(list, acc, pre, post) when is_list(list) do
    {list, acc} = do_traverse_args(list, acc, pre, post)
    post.(list, acc)
  end

  defp do_traverse(x, acc, _pre, post) do
    post.(x, acc)
  end

  defp do_traverse_args(args, acc, _pre, _post) when is_atom(args) do
    {args, acc}
  end

  defp do_traverse_args(args, acc, pre, post) when is_list(args) do
    Enum.map_reduce(args, acc, fn x, acc ->
      {x, acc} = pre.(x, acc)
      do_traverse(x, acc, pre, post)
    end)
  end

  @doc """
  Performs a depth-first, pre-order traversal of quoted expressions.
  """
  @spec prewalk(t, (t -> t)) :: t
  def prewalk(ast, fun) when is_function(fun, 1) do
    elem(prewalk(ast, nil, fn x, nil -> {fun.(x), nil} end), 0)
  end

  @doc """
  Performs a depth-first, pre-order traversal of quoted expressions
  using an accumulator.
  """
  @spec prewalk(t, any, (t, any -> {t, any})) :: {t, any}
  def prewalk(ast, acc, fun) when is_function(fun, 2) do
    traverse(ast, acc, fun, fn x, a -> {x, a} end)
  end

  @doc """
  Performs a depth-first, post-order traversal of quoted expressions.
  """
  @spec postwalk(t, (t -> t)) :: t
  def postwalk(ast, fun) when is_function(fun, 1) do
    elem(postwalk(ast, nil, fn x, nil -> {fun.(x), nil} end), 0)
  end

  @doc """
  Performs a depth-first, post-order traversal of quoted expressions
  using an accumulator.
  """
  @spec postwalk(t, any, (t, any -> {t, any})) :: {t, any}
  def postwalk(ast, acc, fun) when is_function(fun, 2) do
    traverse(ast, acc, fn x, a -> {x, a} end, fun)
  end

  @doc """
  Decomposes a local or remote call into its remote part (when provided),
  function name and argument list.

  Returns `:error` when an invalid call syntax is provided.

  ## Examples

      iex> Macro.decompose_call(quote(do: foo))
      {:foo, []}

      iex> Macro.decompose_call(quote(do: foo()))
      {:foo, []}

      iex> Macro.decompose_call(quote(do: foo(1, 2, 3)))
      {:foo, [1, 2, 3]}

      iex> Macro.decompose_call(quote(do: Elixir.M.foo(1, 2, 3)))
      {{:__aliases__, [], [:Elixir, :M]}, :foo, [1, 2, 3]}

      iex> Macro.decompose_call(quote(do: 42))
      :error

  """
  @spec decompose_call(Macro.t()) :: {atom, [Macro.t()]} | {Macro.t(), atom, [Macro.t()]} | :error
  def decompose_call(ast)

  def decompose_call({{:., _, [remote, function]}, _, args})
      when is_tuple(remote) or is_atom(remote),
      do: {remote, function, args}

  def decompose_call({name, _, args}) when is_atom(name) and is_atom(args), do: {name, []}

  def decompose_call({name, _, args}) when is_atom(name) and is_list(args), do: {name, args}

  def decompose_call(_), do: :error

  @doc """
  Recursively escapes a value so it can be inserted
  into a syntax tree.

  One may pass `unquote: true` to `escape/2`
  which leaves `unquote/1` statements unescaped, effectively
  unquoting the contents on escape.

  ## Examples

      iex> Macro.escape(:foo)
      :foo

      iex> Macro.escape({:a, :b, :c})
      {:{}, [], [:a, :b, :c]}

      iex> Macro.escape({:unquote, [], [1]}, unquote: true)
      1

  """
  @spec escape(term, keyword) :: Macro.t()
  def escape(expr, opts \\ []) do
    elem(:elixir_quote.escape(expr, Keyword.get(opts, :unquote, false)), 0)
  end

  @doc """
  Validates the given expressions are valid quoted expressions.

  Checks the `t:Macro.t/0` for the specification of a valid
  quoted expression.

  It returns `:ok` if the expression is valid. Otherwise it returns a tuple in the form of
  `{:error, remainder}` where `remainder` is the invalid part of the quoted expression.

  ## Examples

      iex> Macro.validate({:two_element, :tuple})
      :ok
      iex> Macro.validate({:three, :element, :tuple})
      {:error, {:three, :element, :tuple}}

      iex> Macro.validate([1, 2, 3])
      :ok
      iex> Macro.validate([1, 2, 3, {4}])
      {:error, {4}}

  """
  @spec validate(term) :: :ok | {:error, term}
  def validate(expr) do
    find_invalid(expr) || :ok
  end

  defp find_invalid({left, right}), do: find_invalid(left) || find_invalid(right)

  defp find_invalid({left, meta, right})
       when is_list(meta) and (is_atom(right) or is_list(right)),
       do: find_invalid(left) || find_invalid(right)

  defp find_invalid(list) when is_list(list), do: Enum.find_value(list, &find_invalid/1)

  defp find_invalid(pid) when is_pid(pid), do: nil
  defp find_invalid(atom) when is_atom(atom), do: nil
  defp find_invalid(num) when is_number(num), do: nil
  defp find_invalid(bin) when is_binary(bin), do: nil

  defp find_invalid(fun) when is_function(fun) do
    unless :erlang.fun_info(fun, :env) == {:env, []} and
             :erlang.fun_info(fun, :type) == {:type, :external} do
      {:error, fun}
    end
  end

  defp find_invalid(other), do: {:error, other}

  @doc ~S"""
  Unescapes the given chars.

  This is the unescaping behaviour used by default in Elixir
  single- and double-quoted strings. Check `unescape_string/2`
  for information on how to customize the escaping map.

  In this setup, Elixir will escape the following: `\0`, `\a`, `\b`,
  `\d`, `\e`, `\f`, `\n`, `\r`, `\s`, `\t` and `\v`. Bytes can be
  given as hexadecimals via `\xNN` and Unicode Codepoints as
  `\uNNNN` escapes.

  This function is commonly used on sigil implementations
  (like `~r`, `~s` and others) which receive a raw, unescaped
  string.

  ## Examples

      iex> Macro.unescape_string("example\\n")
      "example\n"

  In the example above, we pass a string with `\n` escaped
  and return a version with it unescaped.
  """
  @spec unescape_string(String.t()) :: String.t()
  def unescape_string(chars) do
    :elixir_interpolation.unescape_chars(chars)
  end

  @doc ~S"""
  Unescapes the given chars according to the map given.

  Check `unescape_string/1` if you want to use the same map
  as Elixir single- and double-quoted strings.

  ## Map

  The map must be a function. The function receives an integer
  representing the codepoint of the character it wants to unescape.
  Here is the default mapping function implemented by Elixir:

      def unescape_map(unicode), do: true
      def unescape_map(hex), do: true
      def unescape_map(?0), do: ?0
      def unescape_map(?a), do: ?\a
      def unescape_map(?b), do: ?\b
      def unescape_map(?d), do: ?\d
      def unescape_map(?e), do: ?\e
      def unescape_map(?f), do: ?\f
      def unescape_map(?n), do: ?\n
      def unescape_map(?r), do: ?\r
      def unescape_map(?s), do: ?\s
      def unescape_map(?t), do: ?\t
      def unescape_map(?v), do: ?\v
      def unescape_map(e),  do: e

  If the `unescape_map/1` function returns `false`, the char is
  not escaped and the backslash is kept in the string.

  Hexadecimals and Unicode codepoints will be escaped if the map
  function returns `true` for `?x`. Unicode codepoints if the map
  function returns `true` for `?u`.

  ## Examples

  Using the `unescape_map/1` function defined above is easy:

      Macro.unescape_string "example\\n", &unescape_map(&1)

  """
  @spec unescape_string(String.t(), (non_neg_integer -> non_neg_integer | false)) :: String.t()
  def unescape_string(chars, map) do
    :elixir_interpolation.unescape_chars(chars, map)
  end

  @doc false
  def unescape_tokens(tokens) do
    :elixir_interpolation.unescape_tokens(tokens)
  end

  @doc false
  def unescape_tokens(tokens, map) do
    :elixir_interpolation.unescape_tokens(tokens, map)
  end

  @doc """
  Converts the given expression AST to a string.

  The given `fun` is called for every node in the AST with two arguments: the
  AST of the node being printed and the string representation of that same
  node. The return value of this function is used as the final string
  representation for that AST node.

  This function discards all formatting of the original code.

  ## Examples

      iex> Macro.to_string(quote(do: foo.bar(1, 2, 3)))
      "foo.bar(1, 2, 3)"

      iex> Macro.to_string(quote(do: 1 + 2), fn
      ...>   1, _string -> "one"
      ...>   2, _string -> "two"
      ...>   _ast, string -> string
      ...> end)
      "one + two"

  """
  @spec to_string(Macro.t(), (Macro.t(), String.t() -> String.t())) :: String.t()
  def to_string(tree, fun \\ fn _ast, string -> string end)

  # Variables
  def to_string({var, _, atom} = ast, fun) when is_atom(atom) do
    fun.(ast, Atom.to_string(var))
  end

  # Aliases
  def to_string({:__aliases__, _, refs} = ast, fun) do
    fun.(ast, Enum.map_join(refs, ".", &call_to_string(&1, fun)))
  end

  # Blocks
  def to_string({:__block__, _, [expr]} = ast, fun) do
    fun.(ast, to_string(expr, fun))
  end

  def to_string({:__block__, _, _} = ast, fun) do
    block = adjust_new_lines(block_to_string(ast, fun), "\n  ")
    fun.(ast, "(\n  " <> block <> "\n)")
  end

  # Bits containers
  def to_string({:<<>>, _, parts} = ast, fun) do
    if interpolated?(ast) do
      fun.(ast, interpolate(ast, fun))
    else
      result =
        Enum.map_join(parts, ", ", fn part ->
          str = bitpart_to_string(part, fun)

          if :binary.first(str) == ?< or :binary.last(str) == ?> do
            "(" <> str <> ")"
          else
            str
          end
        end)

      fun.(ast, "<<" <> result <> ">>")
    end
  end

  # Tuple containers
  def to_string({:{}, _, args} = ast, fun) do
    tuple = "{" <> Enum.map_join(args, ", ", &to_string(&1, fun)) <> "}"
    fun.(ast, tuple)
  end

  # Map containers
  def to_string({:%{}, _, args} = ast, fun) do
    map = "%{" <> map_to_string(args, fun) <> "}"
    fun.(ast, map)
  end

  def to_string({:%, _, [struct_name, map]} = ast, fun) do
    {:%{}, _, args} = map
    struct = "%" <> to_string(struct_name, fun) <> "{" <> map_to_string(args, fun) <> "}"
    fun.(ast, struct)
  end

  # Fn keyword
  def to_string({:fn, _, [{:->, _, [_, tuple]}] = arrow} = ast, fun)
      when not is_tuple(tuple) or elem(tuple, 0) != :__block__ do
    fun.(ast, "fn " <> arrow_to_string(arrow, fun) <> " end")
  end

  def to_string({:fn, _, [{:->, _, _}] = block} = ast, fun) do
    fun.(ast, "fn " <> block_to_string(block, fun) <> "\nend")
  end

  def to_string({:fn, _, block} = ast, fun) do
    block = adjust_new_lines(block_to_string(block, fun), "\n  ")
    fun.(ast, "fn\n  " <> block <> "\nend")
  end

  # left -> right
  def to_string([{:->, _, _} | _] = ast, fun) do
    fun.(ast, "(" <> arrow_to_string(ast, fun, true) <> ")")
  end

  # left when right
  def to_string({:when, _, [left, right]} = ast, fun) do
    right =
      if right != [] and Keyword.keyword?(right) do
        kw_list_to_string(right, fun)
      else
        fun.(ast, op_to_string(right, fun, :when, :right))
      end

    fun.(ast, op_to_string(left, fun, :when, :left) <> " when " <> right)
  end

  # Splat when
  def to_string({:when, _, args} = ast, fun) do
    {left, right} = :elixir_utils.split_last(args)
    fun.(ast, "(" <> Enum.map_join(left, ", ", &to_string(&1, fun)) <> ") when " <> to_string(right, fun))
  end

  # Capture
  def to_string({:&, _, [{:/, _, [{name, _, ctx}, arity]}]} = ast, fun)
      when is_atom(name) and is_atom(ctx) and is_integer(arity) do
    fun.(ast, "&" <> Atom.to_string(name) <> "/" <> to_string(arity, fun))
  end

  def to_string({:&, _, [{:/, _, [{{:., _, [mod, name]}, _, []}, arity]}]} = ast, fun)
      when is_atom(name) and is_integer(arity) do
    fun.(ast, "&" <> to_string(mod, fun) <> "." <> Atom.to_string(name) <> "/" <> to_string(arity, fun))
  end

  def to_string({:&, _, [arg]} = ast, fun) when not is_integer(arg) do
    fun.(ast, "&(" <> to_string(arg, fun) <> ")")
  end

  # left not in right
  def to_string({:not, _, [{:in, _, [left, right]}]} = ast, fun)  do
    fun.(ast, to_string(left, fun) <> " not in " <> to_string(right, fun))
  end

  # Unary ops
  def to_string({unary, _, [{binary, _, [_, _]} = arg]} = ast, fun)
      when unary in unquote(@unary_ops) and binary in unquote(@binary_ops) do
    fun.(ast, Atom.to_string(unary) <> "(" <> to_string(arg, fun) <> ")")
  end

  def to_string({:not, _, [arg]} = ast, fun)  do
    fun.(ast, "not " <> to_string(arg, fun))
  end

  def to_string({op, _, [arg]} = ast, fun) when op in unquote(@unary_ops) do
    fun.(ast, Atom.to_string(op) <> to_string(arg, fun))
  end

  # Access
  def to_string({{:., _, [Access, :get]}, _, [{op, _, _} = left, right]} = ast, fun)
      when op in unquote(@binary_ops) do
    fun.(ast, "(" <> to_string(left, fun) <> ")" <> to_string([right], fun))
  end

  def to_string({{:., _, [Access, :get]}, _, [left, right]} = ast, fun) do
    fun.(ast, to_string(left, fun) <> to_string([right], fun))
  end

  # All other calls
  def to_string({target, _, args} = ast, fun) when is_list(args) do
    if sigil = sigil_call(ast, fun) do
      sigil
    else
      {list, last} = :elixir_utils.split_last(args)
      fun.(ast, case kw_blocks?(last) do
        true  -> call_to_string_with_args(target, list, fun) <> kw_blocks_to_string(last, fun)
        false -> call_to_string_with_args(target, args, fun)
      end)
    end
  end

  # Two-element tuples
  def to_string({left, right}, fun) do
    to_string({:{}, [], [left, right]}, fun)
  end

  # Lists
  def to_string(list, fun) when is_list(list) do
    fun.(list, cond do
      list == [] ->
        "[]"
      :io_lib.printable_list(list) ->
        IO.iodata_to_binary [?', Inspect.BitString.escape(IO.chardata_to_string(list), ?'), ?']
      Inspect.List.keyword?(list) ->
        "[" <> kw_list_to_string(list, fun) <> "]"
      true ->
        "[" <> Enum.map_join(list, ", ", &to_string(&1, fun)) <> "]"
    end)
  end

  # All other structures
  def to_string(other, fun), do: fun.(other, inspect(other, []))

  defp bitpart_to_string({:::, _, [left, right]} = ast, fun) do
    result =
      op_to_string(left, fun, :::, :left) <>
      "::" <>
      bitmods_to_string(right, fun, :::, :right)
    fun.(ast, result)
  end

  defp bitpart_to_string(ast, fun) do
    to_string(ast, fun)
  end

  defp bitmods_to_string({op, _, [left, right]} = ast, fun, _, _) when op in [:*, :-] do
    result =
      bitmods_to_string(left, fun, op, :left) <>
      Atom.to_string(op) <>
      bitmods_to_string(right, fun, op, :right)
    fun.(ast, result)
  end

  defp bitmods_to_string(other, fun, parent_op, side) do
    op_to_string(other, fun, parent_op, side)
  end

  # Block keywords
  @kw_keywords [:do, :catch, :rescue, :after, :else]

  defp kw_blocks?([{:do, _} | _] = kw) do
    Enum.all?(kw, &match?({x, _} when x in unquote(@kw_keywords), &1))
  end
  defp kw_blocks?(_), do: false

  # Check if we have an interpolated string.
  defp interpolated?({:<<>>, _, [_, | _] = parts}) do
    Enum.all?(parts, fn
      {:::, _, [{{:., _, [Kernel, :to_string]}, _, [_]},
                {:binary, _, _}]} -> true
      binary when is_binary(binary) -> true
      _ -> false
    end)
  end

  defp interpolated?(_) do
    false
  end

  defp interpolate({:<<>>, _, parts}, fun) do
    parts = Enum.map_join(parts, "", fn
      {:::, _, [{{:., _, [Kernel, :to_string]}, _, [arg]}, {:binary, _, _}]} ->
        "\#{" <> to_string(arg, fun) <> "}"
      binary when is_binary(binary) ->
        binary = inspect(binary, [])
        :binary.part(binary, 1, byte_size(binary) - 2)
    end)

    <<?", parts::binary, ?">>
  end

  defp module_to_string(atom, _fun) when is_atom(atom), do: inspect(atom, [])
  defp module_to_string(other, fun), do: call_to_string(other, fun)

  defp sigil_call({func, _, [{:<<>>, _, _} = bin, args]} = ast, fun) when is_atom(func) and is_list(args) do
    sigil =
      case Atom.to_string(func) do
        <<"sigil_", name>> ->
          "~" <> <<name>> <>
          interpolate(bin, fun) <>
          sigil_args(args, fun)
        _ ->
          nil
      end
    fun.(ast, sigil)
  end

  defp sigil_call(_other, _fun) do
    nil
  end

  defp sigil_args([], _fun),  do: ""
  defp sigil_args(args, fun), do: fun.(args, List.to_string(args))

  defp call_to_string(atom, _fun) when is_atom(atom),
    do: Atom.to_string(atom)
  defp call_to_string({:., _, [{:&, _, [val]} = arg]}, fun) when not is_integer(val),
    do: "(" <> module_to_string(arg, fun) <> ")."
  defp call_to_string({:., _, [{:fn, _, _} = arg]}, fun),
    do: "(" <> module_to_string(arg, fun) <> ")."
  defp call_to_string({:., _, [arg]}, fun),
    do: module_to_string(arg, fun) <> "."
  defp call_to_string({:., _, [left, right]}, fun),
    do: module_to_string(left, fun) <> "." <> call_to_string(right, fun)
  defp call_to_string(other, fun),
    do: to_string(other, fun)

  defp call_to_string_with_args(target, args, fun) do
    target = call_to_string(target, fun)
    args = args_to_string(args, fun)
    target <> "(" <> args <> ")"
  end

  defp args_to_string(args, fun) do
    {list, last} = :elixir_utils.split_last(args)

    if last != [] and Inspect.List.keyword?(last) do
      prefix =
        case list do
          [] -> ""
          _  -> Enum.map_join(list, ", ", &to_string(&1, fun)) <> ", "
        end
      prefix <> kw_list_to_string(last, fun)
    else
      Enum.map_join(args, ", ", &to_string(&1, fun))
    end
  end

  defp kw_blocks_to_string(kw, fun) do
    Enum.reduce(@kw_keywords, " ", fn(x, acc) ->
      case Keyword.has_key?(kw, x) do
        true  -> acc <> kw_block_to_string(x, Keyword.get(kw, x), fun)
        false -> acc
      end
    end) <> "end"
  end

  defp kw_block_to_string(key, value, fun) do
    block = adjust_new_lines block_to_string(value, fun), "\n  "
    Atom.to_string(key) <> "\n  " <> block <> "\n"
  end

  defp block_to_string([{:->, _, _} | _] = block, fun) do
    Enum.map_join(block, "\n", fn({:->, _, [left, right]}) ->
      left = comma_join_or_empty_paren(left, fun, false)
      left <> "->\n  " <> adjust_new_lines block_to_string(right, fun), "\n  "
    end)
  end

  defp block_to_string({:__block__, _, exprs}, fun) do
    Enum.map_join(exprs, "\n", &to_string(&1, fun))
  end

  defp block_to_string(other, fun), do: to_string(other, fun)

  defp map_to_string([{:|, _, [update_map, update_args]}], fun) do
    to_string(update_map, fun) <> " | " <> map_to_string(update_args, fun)
  end

  defp map_to_string(list, fun) do
    cond do
      Inspect.List.keyword?(list) -> kw_list_to_string(list, fun)
      true -> map_list_to_string(list, fun)
    end
  end

  defp kw_list_to_string(list, fun) do
    Enum.map_join(list, ", ", fn {key, value} ->
      atom_name = case Inspect.Atom.inspect(key) do
        ":" <> rest -> rest
        other       -> other
      end
      atom_name <> ": " <> to_string(value, fun)
    end)
  end
end
