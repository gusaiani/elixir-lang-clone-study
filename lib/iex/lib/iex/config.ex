defmodule IEx.Config do
  @moduledoc false
  use Agent

  @table __MODULE__
  @agent __MODULE__
  @keys [:colors, :inspect, :history_size, :default_prompt, :alive_prompt, :width]

  # Read API

  def configuration() do
    Application.get_all_env(:iex) |> Keyword.take(@keys)
  end

  def width() do
    columns = columns()
    value = Application.get_env(:iex, :width) || 80
    min(value, columns)
  end

  defp columns() do
    case :io.columns() do
      {:ok, width} -> width
      {:error, _} -> 80
    end
  end

  def started?() do
    Process.whereis(@agent) !== nil
  end

  def history_size() do
    Application.fetch_env!(:iex, :history_size)
  end

  def default_prompt() do
    Application.fetch_env!(:iex, :default_prompt)
  end

  def color(color) do
    color(color, Application.get_env(:iex, :colors, []))
  end

  defp color(color, colors) do
    if colors_enabled?(colors) do
      case Keyword.fetch(colors, color) do
        {:ok, value} ->
          value

        :error ->
          default_color(color)
      end
    else
      nil
    end
  end

  defp colors_enabled?(colors) do
    case Keyword.fetch(colors, :enabled) do
      {:ok, enabled} ->
        enabled

      :error ->
        IO.ANSI.enabled?()
    end
  end

  # Used by default on evaluation cycle
  defp default_color(:eval_interrupt), do: [:yellow]
  defp default_color(:eval_result), do: [:yellow]
  defp default_color(:eval_error), do: [:red]
  defp default_color(:eval_info), do: [:normal]
  defp default_color(:stack_info), do: [:red]
  defp default_color(:blame_diff), do: [:red]

  # Used by ls
  defp default_color(:ls_directory), do: [:blue]
  defp default_color(:ls_device), do: [:green]

  # Used by inspect
  defp default_color(:syntax_colors) do
    [
      atom: :cyan,
      string: :green,
      list: :default_color,
      boolean: :magenta,
      nil: :magenta,
      tuple: :default_color,
      binary: :default_color,
      map: :default_color
    ]
  end

  # Used by ansi docs
  defp default_color(doc_color) do
    IO.ANSI.Docs.default_options() |> Keyword.fetch!(doc_color)
  end

  def inspect_opts() do
    Application.get_env(:iex, :inspect, [])
    |> Keyword.put_new_lazy(:width, &width/0)
    |> update_syntax_colors()
  end

  defp update_syntax_colors(opts) do
    colors = Application.get_env(:iex, :colors, [])

    if syntax_colors = color(:syntax_colors, colors) do
      reset = [:reset | List.wrap(color(:eval_result, colors))]
      syntax_colors = [reset: reset] ++ syntax_colors
      Keyword.update(opts, :syntax_colors, syntax_colors, &Keyword.merge(syntax_colors, &1))
    else
      opts
    end
  end

  # Agent API

  def start_link(_) do
    Agent.start_link(__MODULE__, :handle_init, [], name: @agent)
  end

  def after_spawn(fun) do
    Agent.update(@agent, __MODULE__, :handle_after_spawn, [fun])
  end

  def after_spawn() do
    :ets.lookup_element(@table, :after_spawn, 2)
  end

  def configure(options) do
    Agent.update(@agent, __MODULE__, :handle_configure, [options])
  end
end
