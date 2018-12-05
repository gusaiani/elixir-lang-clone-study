defmodule IEx.Pry do
  @moduledoc """
  The low-level API for prying sessions and setting up breakpoints.
  """

  @doc false
  use GenServer

  @table __MODULE__
  @server __MODULE__
  @timeout :infinity
  @initial_counter 1

  @type id :: integer()
  @type break :: {id, module, {function, arity}, pending :: non_neg_integer}
  @type break_error ::
          :recompilation_failed
          | :no_beam_file
          | :unknown_function_arity
          | :missing_debug_info
          | :outdated_debug_info
          | :non_elixir_module

  @doc """
  Callback for `IEx.pry/0`.

  You can invoke this function directly when you are not able to invoke
  `IEx.pry/0` as a macro. This function expects the binding (from
  `Kernel.binding/0`) and the environment (from `__ENV__/0`).
  """
  def pry(binding, %Macro.Env{} = env) do
    self = self()
    %{file: file, line: line, module: module, function: function_arity} = env
    {:current_stacktrace, stacktrace} = Process.info(self, :current_stacktrace)

    opts = [
      binding: binding,
      dot_iex_path: "",
      env: env,
      prefix: "pry",
      stacktrace: prune_stacktrace(stacktrace)
    ]

    location =
      case function_arity do
        {function, arity} ->
          "#{Exception.format_mfa(module, function, arity)} (#{Path.relative_to_cwd(file)}:#{line})"

        _ ->
          "#{Path.relative_to_cwd(file)}:#{line}"
      end

    whereami =
      case whereami(file, line, 2) do
        {:ok, lines} -> [?\n, ?\n, lines]
        :error -> []
      end

    # If we are the current evaluator, it is because we just
    # reached a pry/breakpoint and the user hit continue().
    # In both cases, we are safe to print and the request will
    # succeed.
    request =
      if Process.get(:iex_evaluator) do
        IO.puts(IEx.color(:eval_interrupt, "Break reached: #{location}#{whereami}"))
        "Prying #{inspect(self)} at #{location}"
      else
        "Request to pry #{inspect(self)} at #{location}#{whereami}"
      end

    # We cannot use colors because IEx may be off
    case IEx.Broker.take_over(request, [evaluator: self()] ++ opts) do
      {:ok, server, group_leader} ->
        IEx.Evaluator.init(:no_ack, server, group_leader, opts)

      {:error, :no_iex} ->
        extra =
          if match?({:win32, _}, :os.type()) do
            " If you are using Windows, you may need to start IEx with the --werl option."
          else
            ""
          end

        message = "Cannot pry #{inspect(self)} at #{location}. Is an IEx shell running?" <> extra
        IO.puts(:stdio, message)
        {:error, :no_iex}

      {:error, _} = error ->
        error
    end
  end

  def pry(binding, opts) when is_list(opts) do
    vars = for {k, _} when is_atom(k) <- binding, do: {k, nil}
    pry(binding, %{:elixir.env_for_eval(opts) | vars: vars})
  end

  @elixir_internals [:elixir, :erl_eval, IEx.Evaluator, IEx.Pry]

  defp prune_stacktrace([{mod, _, _, _} | t]) when mod in @elixir_internals do
    prune_stacktrace(t)
  end

  defp prune_stacktrace([{Process, :info, 2, _} | t]) do
    prune_stacktrace(t)
  end

  defp prune_stacktrace([h | t]) do
    [h | prune_stacktrace(t)]
  end

  defp prune_stacktrace([]) do
    []
  end

  @doc """
  Formats the location for `whereami/3` prying.

  It receives the `file`, `line` and the snippet `radius` and
  returns `{:ok, lines}`, where lines is a list of chardata
  containing each formatted line, or `:error`.

  The actual line is especially formatted in bold.
  """
  def whereami(file, line, radius)
      when is_binary(file) and is_integer(line) and is_integer(radius) and radius > 0 do
    with true <- File.regular?(file),
         [_ | _] = lines <- whereami_lines(file, line, radius) do
      {:ok, lines}
    else
      _ -> :error
    end
  end

  defp whereami_lines(file, line, radius) do
    min = max(line - radius - 1, 0)
    max = line + radius - 1

    file
    |> File.stream!()
    |> Enum.slice(min..max)
    |> Enum.with_index(min + 1)
    |> Enum.map(&whereami_format_line(&1, line))
  end

  defp whereami_format_line({line_text, line_number}, line) do
    gutter = String.pad_leading(Integer.to_string(line_number), 5, " ")

    if line_number == line do
      IO.ANSI.format_fragment([:bright, gutter, ": ", line_text, :normal])
    else
      [gutter, ": ", line_text]
    end
  end

  @doc """
  Sets up a breakpoint on the given module/function/arity.
  """
  @spec break(module, function, arity, pos_integer) :: {:ok, id()} | {:error, break_error()}
  def break(module, function, arity, breaks \\ 1)
      when is_atom(module) and is_atom(function) and is_integer(arity) and arity >= 0 and
             is_integer(breaks) and breaks > 0 do
    break_call(module, function, arity, quote(do: _), breaks)
  end

  @doc """
  Sets up a breakpoint on the given module/function/args with the given `guard`.

  It requires an `env` to be given to make the expansion of the guards.
  """
  @spec break(module, function, [Macro.t()], Macro.t(), Macro.Env.t(), pos_integer) ::
          {:ok, id()} | {:error, break_error()}
  def break(module, function, args, guards, env, breaks \\ 1)
      when is_atom(module) and is_atom(function) and is_list(args) and is_integer(breaks) and
            breaks > 0 do
    condition = build_args_guard_condition(args, guard, env)
    break_call(module, function, length(args), condition, breaks)
  end

  defp break_call(module, function, arity, condition, breaks) do
    GenServer.call(@server, {:break, module, {function, arity}, condition, breaks}, @timeout)
  end

  ## Callbacks

  @impl true
  def handle_call({:break, module, fa, condition, breaks}, _from, counter) do
    # If there is a match for the given module and fa, we
    # use the ref, otherwise we create a new one.
    {ref, counter} =
      case :ets.match_object(@table, {:_, module, fa, :_, :_}) do
        [{ref, _, _, _, _}] -> {ref, counter}
        [] -> {counter, counter + 1}
      end
  end
end
