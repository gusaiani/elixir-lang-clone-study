defmodule Mix.ProjectStack do
  @moduledoc false

  use Agent
  @timeout 30_000

  @typep file    :: binary
  @typep config  :: keyword
  @typep project :: %{name: module, config: config, file: file}

  @spec start_link(keyword) :: {:ok, pid}
  def start_link(_opts) do
    initial = %{stack: [], post_config: [], cache: %{}}
    Agent.start_link fn -> initial end, name: __MODULE__
  end

  @spec push(module, config, file) :: :ok | {:error, file}
  def push(module, config, file) do
    get_and_update fn %{stack: stack} = state ->
      # Consider the first children to always have io_done
      # because we don't need to print anything unless another
      # project takes ahold of the shell.
      io_done? = stack == []

      config  = Keyword.merge(config, state.post_config)
      project = %{name: module, config: config, file: file, pos: length(stack),
                  recursing?: false, io_done: io_done?, configured_applications: []}

      cond do
        file = find_project_named(module, stack) ->
          {{:error, file}, state}
        true ->
          {:ok, %{state | post_config: [], stack: [project | state.stack]}}
      end
    end
  end

  @spec configured_applications([atom]) :: :ok
  def configured_applications(apps) do
    cast fn state ->
      update_in state.stack, fn
        [h | t] -> [%{h | configured_applications: apps} | t]
        []      -> []
      end
    end
  end

  @spec configured_applications() :: [atom]
  def configured_applications() do
    get fn
      %{stack: [h | _]} -> h.configured_applications
      %{stack: []} -> []
    end
  end

  @spec pop() :: project | nil
  def pop do
    get_and_update fn %{stack: stack} = state ->
      case stack do
        [h | t] -> {take(h), %{state | stack: t}}
        [] -> {nil, state}
      end
    end
  end

  @spec peek() :: project | nil
  def peek do
    get fn %{stack: stack} ->
      case stack do
        [h | _] -> take(h)
        [] -> nil
      end
    end
  end

  @spec root((() -> result)) :: result when result: var
  def root(fun) do
    {top, file} =
      get_and_update fn %{stack: stack} = state ->
        {top, [mid | bottom]} = Enum.split_while(stack, &(not &1.recursing?))
        {{top, mid.file}, %{state | stack: [%{mid | recursing?: false} | bottom]}}
      end

    try do
      File.cd! Path.dirname(file), fun
    after
      cast fn %{stack: [mid | bottom]} = state ->
        %{state | stack: top ++ [%{mid | recursing?: true} | bottom]}
      end
    end
  end

  @spec post_config(config) :: :ok
  def post_config(config) do
    cast fn state ->
      %{state | post_config: Keyword.merge(state.post_config, config)}
    end
  end

  @spec printable_app_name() :: atom | nil
  def printable_app_name do
    get_and_update fn %{stack: stack} = state ->
      case stack do
        [] ->
          {nil, state}
        [%{io_done: true} | _] ->
          {nil, state}
        [h | t] ->
          h = %{h | io_done: true}
          t = Enum.map(t, &%{&1 | io_done: false})
          {h.config[:app], %{state | stack: [h | t]}}
      end
    end
  end
