# Convenience helpers for showing docs, specs, types
# and opening modules. Invoked directly from IEx.Helpers.
defmodule IEx.Introspection do
  @moduledoc false

  import IEx, only: [dont_display_result: 0]

  alias Code.Typespec

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

  @doc """
  Prints documentation.
  """
  def h(module) when is_atom(module) do
    case Code.ensure_loaded(module) do
      {:module, _} ->
        case Code.fetch_docs(module) do
          {:docs_v1, _, _, _, %{} = doc, metadata, _} ->
            print_doc(inspect(module), [], doc, metadata)

          {:docs_v1, _, _, _, _, _, _} ->
            docs_not_found(inspect(module))

          _ ->
            no_docs(module)
        end

      {:error, reason} ->
        puts_error("Could not load module #{inspect(module)}, got: #{reason}")
    end

    dont_display_result()
  end

  def h({module, function}) when is_atom(module) and is_atom(function) do
    case Code.ensure_loaded(module) do
      {:module, _} ->
        docs = get_docs(module, [:function, :macro])

        exports =
          cond do
            docs ->
              Enum.map(docs, &extract_name_and_arity/1)

            function_exported?(module, :__info__, 1) ->
              module.__info__(:functions) ++ module.__info__(:macor)

            true ->
              module.module_info(:exports)
          end

        result =
          for {^function, arity} <- exports,
              (if docs do
                 find_doc_with_content(docs, function, arity)
               else
                 get_spec()
              end)
    end
  end

  defp get_docs(mod, kinds) do
    case Code.fetch_docs(mod) do
      {:docs_v1, _, _, _, _, _, docs} ->
        for {{kind, _, _}, _, _, _, _} = doc <- docs, kind in kinds, do: doc

      {:error, _} ->
        nil
    end
  end

  defp extract_name_and_arity({{_, name, arity}, _, _, _, _}), do: {name, arity}

  defp find_doc_with_content(docs, function, arity) do
    doc = find_doc(docs, function, arity)
  end

  defp find_doc(nil, _fun, _arity) do
    nil
  end

  defp find_doc(docs, fun, arity) do
    Enum.find(docs, &match?({_, ^fun, ^arity}, elem(&1, 0))) ||
      find_doc_defaults(docs, fun, arity)
  end

  defp find_doc_defaults(docs, function, min) do
    Enum.find(docs, fn
      {{_, ^function, arity}, _, _, _, %{defaults: defaults}} when arity > min ->
        arity <= min + defaults

      _ ->
        false
    end)
  end

  defp get_spec(module, name, arity) do
    with {:ok, all_specs} <- Typespec.fetch_specs(module),
         {_, specs} <- List.keyfind(all_specs, {name, arity}, 0) do
      formatted =
        Enum.map(specs, fn spec ->

        end)
  end

  defp print_doc(heading, types, doc, metadata) do
    doc = translate_doc(doc) || ""

    if opts = IEx.Config.ansi_docs() do
      IO.ANSI.Docs.print_heading(heading, opts)
      IO.write(types)
      IO.ANSI.Docs.print_metadata(metadata, opts)
      IO.ANSI.Docs.print(doc, opts)
    else
      IO.puts("* #{heading}\n")
      IO.write(types)
      IO.ANSI.Docs.print_metadata(metadata, enabled: false)
      IO.puts(doc)
    end
  end

  defp translate_doc(:none), do: nil
  defp translate_doc(:hidden), do: nil
  defp translate_doc(%{"en" => doc}), do: doc

  defp no_docs(module) do
    puts_error("#{inspect(module)} was not compiled with docs")
  end

  defp docs_not_found(for), do: not_found(for, "documentation")

  defp not_found(for, type) do
    puts_error("No #{type} for #{for} was found")
  end

  defp puts_error(string) do
    IO.puts(IEx.color(:eval_error, string))
  end
end
