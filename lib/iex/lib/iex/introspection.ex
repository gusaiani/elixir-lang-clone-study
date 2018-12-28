# Convenience helpers for showing docs, specs, types
# and opening modules. Invoked directly from IEx.Helpers.
defmodule IEx.Introspection do
  @moduledoc false

  import IEx, only: [dont_display_result: 0]

  @doc """
  Decomposes an introspection call into `{mod, fun, arity}`,
  `{mod, fun}` or `mod`.
  """
  def decompose({:/, _, [call, arity]} = term, context) do
    case Macro.decompose_call(call) do
      {_mod, :__info__, []} when arity == 1 ->
        {:{}, [], [Module, :__info__, 1]}

      {mod, fun, []} ->
        {:{}, [], [mod, fun, arity]}

      {fun, []} ->
        {:{}, [], [find_decompose_fun_arity(fun, arity, context), fun, arity]}

      _ ->
        term
    end
  end

  def decompose(call, context) do
    case Macro.decompose_call(call) do
      {_mod, :__info__, []} ->
        Macro.escape({Module, :__info__, 1})

      {mod, fun, []} ->
        {mod, fun}

      {fun, []} ->
        {find_decompose_fun(fun, context), fun}

      _ ->
        call
    end
  end

  defp find_decompose_fun(fun, context) do
    find_import(fun, context.functions) || find_import(fun, context.macros) ||
      find_special_form(fun) || Kernel
  end

  defp find_decompose_fun_arity(fun, arity, context) do
    pair = {fun, arity}

    find_import(pair, context.functions) || find_import(pair, context.macros) ||
      find_special_form(pair) || Kernel
  end

  defp find_import(pair, context) when is_tuple(pair) do
    Enum.find_value(context, fn {mod, functions} ->
      if pair in functions, do: mod
    end)
  end

  defp find_import(fun, context) do
    Enum.find_value(context, fn {mod, functions} ->
      if Keyword.has_key?(functions, fun), do: mod
    end)
  end

  @doc """
  Opens the given module, mfa, file/line, binary.
  """
  def open(module) when is_atom(module) do
    case open_mfa(module, :__info__, 1) do
      {source, nil, _} -> open(source)
      {_, tuple, _} ->
    end
  end
end
