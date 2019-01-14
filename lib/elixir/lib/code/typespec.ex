defmodule Code.Typespec do
  @moduledoc false

  @doc """
  Returns all specs available from the module's BEAM code.

  The result is returnd as a list of tuples where the first
  element is spec name and arity and the second is the spec.

  The module must have a corresponding BEAM file which can be
  located by the runtime system. The types will be in the Erlang   Abstract Format.
  """
  @spec fetch_specs(module) :: {:ok, [tuple]} | :error
  def fetch_specs(module) when is_atom(module) or is_binary(module) do
    case typespecs_abstract_code(module) do
      {:ok, abstract_code} ->
        {:ok, for({:attribute, _, :spec, value} <- abstract_code, do: value)}

      :error ->
        :error
    end
  end

  defp typespecs_abstract_code(module) do
    with {module, binary} <- get_module_and_beam(module),
         {:ok, {_, [debug_info: {:debug_info_v1, backend, data}]}} <-
           :beam_lib.chunks(binary, [:debug_info]) do
      case data do
        {:elixir_v1, %{}, specs} ->
          # Fast path to avoid translation to Erlang from Elixir.
          {:ok, specs}

        _ ->
          case backend.debug_info(:erlang_v1, module, data, []) do
            {:ok, abstact_code} -> {:ok, abstract_code}
            _ -> :error
          end
      end
    else
      _ -> :error
    end
  end

  defp get_module_and_beam(module) when is_atom(module) do
    case :code.get_object_code(module) do
      {^module, beam, _filename} -> {module, beam}
      :error -> :error
    end
  end

  defp get_module_and_beam(beam) when is_binary(beam) do
    case :beam_lib.info(beam) do
      [_ | _] = info -> {info[:module], beam}
      _ -> :error
    end
  end

  defp typespec_to_quoted({:user_type, line, name, args}) do
    typespec_to_quoted({:type, line, name, args})
  end

  defp typespec_to_quoted({:type, line, :tuple, :any}) do
    {:tuple, [line: line], []}
  end

  defp typespec_to_quoted({:type, line, :tuple, args}) do
    args = for arg <- args, do: typespec_to_quoted(arg)
    {:{}, [line: line], args}
  end

  defp typespec_to_quoted({:type, _line, :list, [{:type, _, :union, unions} = arg]}) do
    case unpack_typespec_kw(unions, []) do
      {:ok, ast} -> ast
      :error -> [typespect_to_quoted(arg)]
    end
  end

  defp typespec_to_quoted({:type, line, :list, []}) do
    {:list, [line: line], []}
  end

  defp typespec_to_quoted({:type, _line, :list, [arg]}) do
    [typespec_to_quoted(arg)]
  end

  defp typespec_to_quoted({:type, line, :nonempty_list, []}) do
    [{:..., [line: line], nil}]
  end

  defp typespec_to_quoted({:type, line, :nonempty_list, [arg]}) do
    [typespec_to_quoted(arg), {:..., [line: line], nil}]
  end

  defp unpack_typespec_kw([{:type, _, :tuple, [{:atom, _, atom}, type]} | t], acc) do
    unpack_typespec_kw(t, [{atom, typespec_to_quoted(type)} | acc])
  end

  defp unpack_typespec_kw([], acc) do
    {:ok, Enum.reverse(acc)}
  end

  defp unpack_typespec_kw(_, _acc) do
    :error
  end
end


