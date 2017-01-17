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

  @typedoc "Abstract Syntax Tree (AST)"
  @type t :: expr | {t, t} | atom | number | binary | pid | fun | [t]
  @type expr :: {expr | atom, Keyword.t, atom | [t]}

  @binary_ops [:===, :!==,
    :==, :!=, :<=, :>=,
    :&&, :||, :<>, :++, :--, :\\, :::, :<-, :.., :|>, :=~,
    :<, :>, :->,
    :+, :-, :*, :/, :=, :|, :.,
    :and, :or, :when, :in,
    :~>>, :<<~, :~>, :<~, :<~>, :<|>,
    :<<<, :>>>, :|||, :&&&, :^^^, :~~~]

  @doc false
  defmacro binary_ops, do: @binary_ops

  @unary_ops [:!, :@, :^, :not, :+, :-, :~~~, :&]

  @doc false
  defmacro unary_ops, do: @unary_ops

  @spec binary_op_props(atom) :: {:left | :right, precedence :: integer}
  defp binary_op_props(o) do
    case o do
      o when o in [:<-, :\\]                  -> {:left,  40}
      :when                                   -> {:right, 50}
      :::                                     -> {:right, 60}
      :|                                      -> {:right, 70}
      :=                                      -> {:right, 90}
      o when o in [:||, :|||, :or]            -> {:left, 130}
      o when o in [:&&, :&&&, :and]           -> {:left, 140}
      o when o in [:==, :!=, :=~, :===, :!==] -> {:left, 150}
      o when o in [:<, :<=, :>=, :>]          -> {:left, 160}
      o when o in [:|>, :<<<, :>>>, :<~, :~>,
                :<<~, :~>>, :<~>, :<|>, :^^^] -> {:left, 170}
      :in                                     -> {:left, 180}
      o when o in [:++, :--, :.., :<>]        -> {:right, 200}
      o when o in [:+, :-]                    -> {:left, 210}
      o when o in [:*, :/]                    -> {:left, 220}
      :.                                      -> {:left, 310}
    end
  end

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
  @spec unpipe(Macro.t) :: [Macro.t]
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
  @spec pipe(Macro.t, Macro.t, integer) :: Macro.t | no_return
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

  def pipe(expr, {call, _, [_, _]} = call_args, _integer)
      when call in unquote(@binary_ops) do
    raise ArgumentError, "cannot pipe #{to_string expr} into #{to_string call_args}," <>
                         "the #{to_string call} operator can only take two arguments"
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

  def pipe(expr, {call, line, args}, integer) when is_list(args) do
    {call, line, List.insert_at(args, integer, expr)}
  end

  def pipe(expr, call_args, _integer) do
    raise ArgumentError, bad_pipe(expr, call_args)
  end

  defp bad_pipe(expr, call_args) do
    "cannot pipe #{to_string expr} into #{to_string call_args}, " <>
    "can only pipe into local calls foo(), remote calls Foo.bar() or anonymous functions calls foo.()"
  end

  @doc false
  def pipe_warning({call, _, _}) when call in unquote(@unary_ops) do
    "piping into a unary operator is deprecated. You could use e.g. Kernel.+(5) instead of +5"
  end
  def pipe_warning(_), do: nil

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
  @spec update_meta(t, (Keyword.t -> Keyword.t)) :: t
  def update_meta(quoted, fun)

  def update_meta({left, meta, right}, fun) when is_list(meta) do
    {left, fun.(meta), right}
  end

  def update_meta(other, _fun) do
    other
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
    do_traverse(st, ac, pre, post)
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
    traverse(ast, acc, fun, fn x, a, -> {x, a} end)
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
  @spec decompose_call(Macro.t) :: {atom, [Macro.t] | {Macro.t, atom, [Macro.t]} | :error}
  def decompose_call(ast)

  def decompose_call({{:., _, [remote, function]}, _, args}) when is_tuple(remote) or is_atom(remote),
    do: {remote, function, args}

  def decompose_call({name, _, args}) when is_atom(name) and is_atom(args),
    do: {name, []}

  def decompose_call({name, _, args}) when is_atom(name) and is_list(args),
    do: {name, args}

  def decompose_call(_),
    do: :error

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
  @spec escape(term) :: Macro.t
  @spec escape(term, Keyword.t) :: Macro.t
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

  defp find_invalid({left, right}), do:
    find_invalid(left) || find_invalid(right)

  defp find_invalid({left, meta, right}) when is_list(meta) and (is_atom(right) or is_list(right)), do:
    find_invalid(left) || find_invalid(right)

  defp find_invalid(list) when is_list(list), do:
    Enum.find_value(list, &find_invalid/1)

  defp find_invalid(pid)  when is_pid(pid),    do: nil
  defp find_invalid(atom) when is_atom(atom),  do: nil
  defp find_invalid(num)  when is_number(num), do: nil
  defp find_invalid(bin)  when is_binary(bin), do: nil

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
  @spec unescape_string(String.t) :: String.t
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
      def unescape_map(?x), do: true
      def unescape_map(?u), do: true
      def unescape_map(e),  do: e

  If the `unescape_map/1` function returns `false`, the char is
  not escaped and the backslash is kept in the string.

  Hexadecimals and Unicode codepoints will be escaped if the map
  function returns `true` for `?x`. Unicode codepoints if the map
  function return `true` for `?u`.

  ## Examples

  Using the `unescape_map/1` function defined above is easy:

      Macro.unescape_string "example\\n", &unescape_map(&1)

  """
  @spec unescape_string(String.t, (non_neg_integer -> non_neg_integer | false)) :: String.t
  def unescape_string(chars, map) do
    :elixir_interpolation.unescape_chars(chars, map)
  end

  @doc """
  Unescapes the given tokens according to the default map.

  Check `unescape_string/1` and `unescape_string/2` for more
  information about unescaping.

  Only tokens that are binaries are unescaped, all others are
  ignored. This function is useful when implementing your own
  sigils. Check the implementation of `Kernel.sigil_s/2`
  for examples.
  """
  @spec unescape_tokens([Macro.t]) :: [Macro.t]
  def unescape_tokens(tokens) do
    :elixir_interpolation.unescape_tokens(tokens)
  end

  @doc """
  Unescapes the given tokens according to the given map.

  Check `unescape_tokens/1` and `unescape_string/2` for more information.
  """
  @spec unescape_tokens([Macro.t], (non_neg_integer -> non_neg_integer | false)) :: [Macro.t]
  def unescape_tokens(tokens, map) do
    :elixir_interpolation.unescape_tokens(tokens, map)
  end

  @doc """
  Converts the given expression to a binary.

  The given `fun` is called for every node in the AST with two arguments: the
  AST of the node being printed and the string representation of that same
  node. The return value of this function is used as the final string
  representation for that AST node.

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
  @spec to_string(Macro.t) :: String.t
  @spec to_string(Macro.t, (Macro.t, String.t -> String.t)) :: String.t
  def to_string(tree, fun \\ fn(_ast, string) -> string end)
end
