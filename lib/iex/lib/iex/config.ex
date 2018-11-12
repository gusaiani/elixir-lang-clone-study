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
