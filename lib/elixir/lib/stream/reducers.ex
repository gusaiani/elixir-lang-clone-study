defmodule Stream.Reducers do
  # Collection of reducers and utilities shared by Enum and Stream.
  @moduledoc false

  def chunk_every(chunk_by, enumerable, count, step, leftover) do
    limit = :erlang.max(count, step)

    chunk_fun = fn entry, {acc_buffer, acc_count} ->
      acc_buffer = [entry | acc_buffer]
      acc_count = acc_count + 1

      new_state =
        if acc_count >= limit do
          remaining = acc_count - step
          {Enum.take(acc_buffer, remaining), remaining}
        else
          {acc_buffer, acc_count}
        end

      if acc_count == count do
        {:cont, :lists.reverse(acc_buffer), new_state}
      else
        {:cont, new_state}
      end
    end

    after_fun = fn {acc_buffer, acc_count} ->
      if leftover == :discard or acc_count == 0 do
        {:cont, []}
      else
        {:cont, :lists.reverse(acc_buffer, Enum.take(leftover, count - acc_count)), []}
      end
    end

    chunk_by.(enumerable, {[], 0}, chunk_fun, after_fun)
  end

  def chunk_by(chunk_by, enumerable, fun) do
    chunk_fun = fn
      entry, nil ->
        {:cont, {[entry], fun.(entry)}}

      entry, {acc, value} ->
        case fun.(entry) do
          ^value -> {:cont, {[entry | acc], value}}
          new_value -> {:cont, :lists.reverse(acc), {[entry], new_value}}
        end
    end

    after_fun = fn
      nil -> {:cont, :done}
      {acc, _value} -> {:cont, :lists.reverse(acc), :done}
    end

    chunk_by.(enumerable, nil, chunk_fun, after_fun)
  end

  defmacro chunk_while(callback, fun \\ nil) do
    quote do
      fn entry, acc(head, acc, tail) ->
        case unquote(callback).(entry, acc) do
          {:cont, emit, acc} -> next_with_acc(unquote(fun), emit, head, acc, tail)
          {:cont, acc} -> skip(acc(head, acc, tail))
          {:halt, acc} -> {:halt, acc(head, acc, tail)}
        end
      end
    end
  end

  defmacro dedup(callback, fun \\ nil) do
    quote do
      fn entry, acc(head, prev, tail) = acc ->
        value = unquote(callback).(entry)

        case prev do
          {:value, ^value} -> skip(acc)
          _ -> next_with_acc(unquote(fun), entry, head, {:value, value}, tail)
        end
      end
    end
  end

  defmacro drop(fun \\ nil) do
    quote do
      fn
        _entry, acc(head, amount, tail) when amount > 0 ->
          skip(acc(head, amount - 1, tail))

        entry, acc(head, amount, tail) ->
          next_with_acc(unquote(fun), entry, head, amount, tail)
      end
    end
  end
