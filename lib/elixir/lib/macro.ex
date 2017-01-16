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
end
