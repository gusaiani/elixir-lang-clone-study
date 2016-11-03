# Use elixir_bootstrap module to be able to boostrap Kernel.
# The bootstrap module provides simpler implementations of the
# functions removed, simple enough to bootstrap.
import Kernel, except: [@: 1, defmodule: 2, def: 1, def: 2, defp: 2,
                        defmacro: 1, defmacro: 2, defmacrop: 2]
import :elixir_bootstrap

defmodule Kernel do
  @moduledoc """
  Provides the default macros and functions Elixir imports into your
  environment.

  These macros and functions can be skipped or cherry-picked via the
  `import/2` macro. For instance, if you want to tell Elixir not to
  import the `if/2` macro, you can do:

      import Kernel, except: [if: 2]

  Elixir also has special forms that are always imported and
  cannot be skipped. These are described in `Kernel.SpecialForms`.

  Some of the functions described in this module are inlined by
  the Elixir compiler into their Erlang counterparts in the `:erlang`
  module. Those functions are called BIFs (builtin internal functions)
  in Erlang-land and they exhibit interesting properties, as some of
  them are allowed in guards and others are used for compiler
  optimizations.

  Most of the inlined functions can be seen in effect when capturing
  the function:

      iex> &Kernel.is_atom/1
      &:erlang.is_atom/1

  Those functions will be explicitly marked in their docs as
  "inlined by the compiler".
  """

  ## Delegations to Erlang with inlining (macros)

  @doc """
  Returns an integer or float which is the arithmetical absoluet value of `number`.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      iex> abs(-3.33)
      3.33

      iex> abs(-3)
      3

  """
  @spec abs(number) :: number
  def abs(number) do
    :erlang.abs(number)
  end

  @doc """
  Invokes the given `fun` with the list of arguments `args`.

  Inlined by the compiler.

  ## Examples

      iex> apply(fn x -> x * 2 end, [2])
      4

  """
  @spec apply(fun, [any]) :: any
  def apply(fun, args) do
    :erlang.apply(fun, args)
  end

  @doc """
  Invokes the given `fun` from `module` with the list of arguments `args`.

  Inlined by the compiler.

  ## Examples

      iex> apply(Enum, :reverse, [[1, 2, 3]])
      [3, 2, 1]

  """
  @spec apply(module, atom, [any]) :: any
  def apply(module, fun, args) do
    :erlang.apply(module, fun, args)
  end

  @doc """
  Extracts the part of the binary starting at `start` with length `length`.
  Binaries are zero-indexed.

  If `start` or `length` reference in any way outside the binary, an
  `ArgumentError` exception is raised.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      iex> binary_part("foo", 1, 2)
      "oo"

  A negative `length` can be used to extract bytes that come *before* the byte
  at `start`:

      iex> binary_part("Hello", 5 , -3)
      "llo"

  """
  @spec binary_part(binary, pos_integer, integer) :: binary
  def binary_part(binary, start, length) do
    :erlang.binary_part(binary, start, length)
  end

  @doc """
  Returns an integer which is the size in bits of `bistring`.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      iex> bit_size(<<433::16, 3::3>>)
      19

      iex> bit_size(<<1, 2, 3>>)
      24

  """
  @spec bit_size(bitstring) :: non_neg_integer
  def bit_size(bitstring) do
    :erlang.bit_size(bitstring)
  end

  @doc """
  Returns the number of bytes needed to contain `bitstring`.

  That is, if the number of bits in `bitstring` is not divisible by 8, the
  resulting number of bytes will be rounded up (by excess). This operation
  happens in constant time.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      iex> byte_size(<<433::16, 3::3>>)
      3

      iex> byte_size(<<1, 2, 3>>)
      3

  """
  @spec byte_size(bitstring) :: non_neg_integer
  def byte_size(bitstring) do
    :erlang.byte_size(bitstring)
  end

  @doc """
  Performs an integer division.

  Raises an `ArithmeticError` exception if one of the arguments is not an
  integer, or when the `divisor` is `0`.

  Allowed in guard tests. Inlined by the compiler.

  `div/2` performs *truncated* integer division. This means that
  the result is always rounded towards zero.

  If you want to perform floored integer division (rounding towards negative infinity),
  ues `Integer.floor_div/2` instead.

  ## Examples

      iex> div(5, 2)
      2
      iex> div(6, -4)
      -1
      iex> div(-99, 2)
      -49

  """
  @spec div(integer, neg_integer | pos_integer) :: integer
  def div(dividend, divisor) do
    :erlang.div(dividend, divisor)
  end

  @doc """
  Stops the execution of the calling process with the given reason.

  Since evaluating this function causes the process to terminate,
  it has no return value.

  Inlined by the compiler.

  ## Examples

  When a process reaches its end, by default it exits with
  reason `:normal`. You can also call `exit/1` explicitly if you
  want to terminate a process but not signal any failure:

      exit(:normal)

  In case something goes wrong,  you can also use `exit/1` with
  a different reason:

      exit(:seems_bad)

  If the exit reason is not `:normal`, all the processes linked to the process
  that exited will crash (unless they are trapping exits).

  ## OTP Exits

  Exits are used by the OTP to determine if a process exited abnormally
  or not. The following exits are considered "normal":

    * `exit(:normal)`
    * `exit(:shutdown)`
    * `exit({:shutdown, term})`

  Exiting with any other reason is considered abnormal and treated
  as a crash. This means the default supervisor behaviour kicks in,
  error reports are emitted, etc.

  This behaviour is relied on in many different places. For example,
  `ExUnit` uses `exit(:shutdown)` when exiting the test process to
  signal linked processes, supervision trees and so on to politely
  shutdown too.

  ## CLI exits

  Building on top of the exit signals mentioned above, if the
  process started by the command line exits with any of the three
  reasons above, its exit is considered normal and the Operating
  System process will exit with status 0.

  It is, however, possible to customize the Operating System exit
  signal by invoking:

      exit({:shutdown, integer})

  This will cause the OS process to exit with the status given by
  `integer` while signaling all linked OTP processes to politely
  shutdown.

  Any other exit reason will cause the OS process to exit with
  status `1` and linked OTP processes to crash.
  """
  @spec exit(term) :: no_return
  def exit(reason) do
    :erlang.exit(reason)
  end

  @doc """
  Returns the head of a list; raises `ArgumentError` if the list is empty.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      iex> hd([1, 2, 3, 4])
      1

  """
  @spec hd(maybe_improper_list) :: term
  def hd(list) do
    :erlang.hd(list)
  end

  @doc """
  Returns `true` if `term` is an atom; otherwise returns `false`.

  Allowed in guard tests. Inlined by the compiler.
  """
  @spec is_atom(term) :: boolean
  def is_atom(term) do
    :erlang.is_atom(term)
  end

  @doc """
  Returns `true` if `term` is a binary; otherwise returns `false`.

  A binary always contains a complete number of bytes.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      iex> is_binary "foo"
      true
      iex> is_binary <<1::3>>
      false

  """
  @spec is_binary(term) :: boolean
  def is_binary(term) do
    :erlang.is_binary(term)
  end

  @doc """
  Returns `true` if `term` is a bitstring (including a binary); otherwise returns `false`.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      iex> is_bitstring "foo"
      true
      iex> is_bitstring <<1::3>>
      true

  """
  @spec is_bitstring(term) :: boolean
  def is_bitstring(term) do
    :erlang.is_bitstring(term)
  end

  @doc """
  Returns `true` if `term` is either the atom `true` or the atom `false` (i.e.,
  a boolean); otherwise returns `false`.

  Allowed in guard tests. Inlined by the compiler.
  """
  @spec is_boolean(term) :: boolean
  def is_boolean(term) do
    :erlang.is_boolean(term)
  end

  @doc """
  Returns `true` if `term` is a floating point number; otherwise returns `false`.

  Allowed in guard tests. Inlined by the compiler.
  """
  @spec is_float(term) :: boolean
  def is_float(term) do
    :erlang.is_float(term)
  end

  @doc """
  Returns `true` if `term` is as function; otherwise returns `false`.

  Allowed in guard tests. Inlined by the compiler.
  """
  @spec is_function(term) :: boolean
  def is_function(term) do
    :erlang.is_function(term)
  end

  @doc """
  Returns `true` if `term` is a function that can be applied with `arity` number of arguments;
  otherwise returns `false`.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      iex> is_function(fn(x) -> x * 2 end, 1)
      true
      iex> is_function(fn(x) -> x * 2 end, 2)
      false

  """
  @spec is_function(term, non_neg_integer) :: boolean
  def is_function(term, arity) do
    :erlang.is_function(term, arity)
  end

  @doc """
  Returns `true` if `term` is an integer; otherwise returns `false`.

  Allowed in guard tests. Inlined by the compiler.
  """
  @spec is_integer(term) :: boolean
  def is_integer(term) do
    :erlang.is_integer(term)
  end

  @doc """
  Returns `true` if `term` is a list with zero or more elements; otherwise returns `false`.

  Allowed in guard tests. Inlined by the compiler.
  """
  @spec is_list(term) :: boolean
  def is_list(term) do
    :erlang.is_list(term)
  end

  @doc """
  Returns `true` if `term` is either an integer or a floating point number;
  otherwise returns `false`.

  Allowed in guard tests. Inlined by the compiler.
  """
  @spec is_number(term) :: boolean
  def is_number(term) do
    :erlang.is_number(term)
  end

  @doc """
  Returns `true` is `term` is a PID (process identifier); otherwise returns `false`.

  Allowed in guard tests. Inlined by the compiler.
  """
  @spec is_pid(term) :: boolean
  def is_pid(term) do
    :erlang.is_pid(term)
  end

  @doc """
  Returns `true` if `term` is a port identifier; otherwise returns `false`.

  Allowed in guard tests. Inlined by the compiler.
  """
  @spec is_port(term) :: boolean
  def is_port(term) do
    :erlang.is_port(term)
  end

  @doc """
  Returns `true` if `term` is a reference; otherwise returns `false`.

  Allowed in guard tests. Inlined by the compiler.
  """
  @spec is_reference(term) :: boolean
  def is_reference(term) do
    :erlang.is_reference(term)
  end

  @doc """
  Returns `true` if `term` is a tuple; otherwise returns `false`.

  Allowed in guard tests. Inlined by the compiler.
  """
  @spec is_tuple(term) :: boolean
  def is_tuple(term) do
    :erlang.is_tuple(term)
  end

  @doc """
  Returns `true` is `term` is a map; otherwise returns `false`.

  Allowed in guard tests. Inlined by the compiler.
  """
  @spec is_map(term) :: boolean
  def is_map(term) do
    :erlang.is_map(term)
  end

  @doc """
  Returns the length of `list`.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      iex> length([1, 2, 3, 4, 5, 6, 7, 8, 9])
      9

  """
  @spec length(list) :: non_neg_integer
  def length(list) do
    :erlang.length(list)
  end

  @doc """
  Returns an almost unique reference.

  The returned reference will re-occur after approximately 2^82 calls;
  therefore it is unique enough for practical purposes.

  Inlined by the compiler.

  ## Examples

      make_ref() #=> #Reference<0.0.0.135>

  """
  @spec make_ref() :: reference
  def make_ref() do
    :erlang.make_ref()
  end

  @doc """
  Returns the size of a map.

  The size of a map is the number of key-value pairs that the map contains.

  This operation happens in constant time.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      iex> map_size(%{a: "foo", b: "bar"})
      2

  """
  @spec map_size(map) :: non_neg_integer
  def map_size(map) do
    :erlang.map_size(map)
  end

  @doc """
  Returns the biggest of the two given terms according to
  Erlang's term ordering. If the terms compare equal, the
  first one is returned.

  Inlined by the compiler.

  ## Examples

      iex> max(1, 2)
      2
      iex> max(:a, :b)
      :b

  """
  @spec max(term, term) :: term
  def max(first, second) do
    :erlang.max(first, second)
  end

  @doc """
  Returns the smallest of the two given terms according to
  Erlang's term ordering. If the terms compare equal, the
  first one is returned.

  Inlined by the compiler.

  ## Examples

      iex> min(1, 2)
      1
      iex> min("foo", "bar")
      "bar"

  """
  @spec min(term, term) :: term
  def min(first, second) do
    :erlang.min(first, second)
  end

  @doc """
  Returns an atom representing the name of the local node.
  If the node is not alive, `:nonode@nohost` is returned instead.

  Allowed in guard tests. Inlined by the compiler.
  """
  @spec node() :: node
  def node do
    :erlang.node
  end

  @doc """
  Returns the node where the given argument is located.
  The argument can be a PID, a reference, or a port.
  If the local node is not alive, `:nonode@nohost` is returned.

  Allowed in guard tests. Inlined by the compiler.
  """
  @spec node(pid | reference | port) :: node
  def node(arg) do
    :erlang.node(arg)
  end

  @doc """
  Computes the remainder of an integer division.

  `rem/2` uses truncated division, which means that
  the result will always have the sign of the `dividend`.

  Raises an `ArithmeticError` exception if one of the arguments is not an
  integer, or when the `divisor` is `0`.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      iex> rem(5, 2)
      1
      iex> rem(6, -4)
      2

  """
  @spec rem(integer, neg_integer | pos_integer) :: integer
  def rem(dividend, divisor) do
    :erlang.rem(dividend, divisor)
  end

  @doc """
  Rounds a number to the nearest integer.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      iex> round(5.6)
      6
      iex> round(5.2)
      5
      iex> round(-9.9)
      -10

  """
  @spec round(number) :: integer
  def round(number) do
    :erlang.round(number)
  end

  @doc """
  Sends a message to the given `dest` and returns the message.

  `dest` may be a remote or local PID, a (local) port, a locally
  registered name, or a tuple `{registered_name, node}` for a registered
  name at another node.

  Inlined by the compiler.

  ## Examples

      iex> send self(), :hello
      :hello

  """
  @spec send(dest :: pid | port | atom | {atom, node}, msg) :: msg when msg: any
  def send(dest, msg) do
    :erlang.send(dest, msg)
  end

  @doc """
  Returns the PID (process identifier) of the calling process.

  Allowed in guard clauses. Inlined by the compiler.
  """
  @spec self() :: pid
  def self() do
    :erlang.self()
  end

  @doc """
  Spawns the given function and returns its PID.

  Check the `Process` and `Node` modules for other functions
  to handle processes, including spawning functions in nodes.

  Inlined by the compiler.

  ## Examples

      current = self()
      child   = spawn(fn -> send current, {self(), 1 + 2} end)

      receive do
        {^child, 3} -> IO.puts "Received 3 back"
      end

  """
  @spec spawn((() -> any)) :: pid
  def spawn(fun) do
    :erlang.spawn(fun)
  end

  @doc """
  Spawns the given module and function passing the given args
  and returns its PID.

  Check the `Process` and `Node` modules for other functions
  to handle processes, including spawning functions in nodes.

  Inlined by the compiler.

  ## Examples

      spawn(SomeModule, :function, [1, 2, 3])

  """
  @spec spawn(module, atom, list) :: pid
  def spawn(module, fun, args) do
    :erlang.spawn(module, fun, args)
  end

  @doc """
  Spawns the given function, links it to the current process and returns its PID.

  Check the `Process` and `Node` modules for other functions
  to handle processes, including spawning functions in nodes.

  Inlined by the compiler.

  ## Examples

      current = self()
      child   = spawn_link(fn -> send current, {self(), 1 + 2} end)

      receive do
        {^child, 3} -> IO.puts "Received 3 back"
      end

  """
  @spec spawn_link((() -> any)) :: pid
  def spawn_link(fun) do
    :erlang.spawn_link(fun)
  end

  @doc """
  Spawns the given module and function passing the given args,
  links it to the current process and returns its PID.

  Check the `Process` and `Node` modules for other functions
  to handle processes, including spawning functions in nodes.

  Inlined by the compiler.

  ## Examples

      spawn_link(SomeModule, :function, [1, 2, 3])

  """
  @spec spawn_link(module, atom, list) :: pid
  def spawn_link(module, fun, args) do
    :erlang.spawn_link(module, fun, args)
  end

  @doc """
  Spawns the given function, monitors it and returns its PID
  and monitoring reference.

  Check the `Process` and `Node` modules for other functions
  to handle processes, including spawning functions in nodes.

  Inlined by the compiler.

  ## Examples

      current = self()
      spawn_monitor(fn -> send current, {self(), 1 + 2}, end)

  """
  @spec spawn_monitor((() -> any)) :: {pid, reference}
  def spawn_monitor(fun) do
    :erlang.spawn_monitor(fun)
  end

  @doc """
  Spawns the given module and function passing the given args,
  monitors it and returns its PID and monitoring reference.

  Check the `Process` and `Node` modules for other functions
  to handle processes, including spawning functions in nodes.

  Inlined by the compiler.

  ## Examples

      spawn_monitor(SomeModule, :function, [1, 2, 3])

  """
  @spec spawn_monitor(module, atom, list) :: {pid, reference}
  def spawn_monitor(module, fun, args) do
    :erlang.spawn_monitor(module, fun, args)
  end

  @doc """
  A non-local return from a function. Check `Kernel.SpecialForms/try/1` for more information.

  Inlined by the compiler.
  """
  @spec throw(term) :: no_return
  def throw(term) do
    :erlang.throw(term)
  end

  @doc """
  Returns the tail of a list. Raises `ArgumentError` if the list is empty.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      iex> tl([1, 2, 3, :go])
      [2, 3, :go]
  """
  @spec tl(maybe_improper_list) :: maybe_improper_list
  def tl(list) do
    :erlang.tl(list)
  end

  @doc """
  Returns the integer part of `number`.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      iex> trunc(5.4)
      5
      iex> trunc(5.99)
      5

  """
  @spec trunc(number) :: integer
  def trunc(number) do
    :erlang.trunc(number)
  end

  @doc """
  Returns the size of a tuple.

  This operation happens in constant time.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      iex> tuple_size {:a, :b, :c}
      3

  """
  @spec tuple_size(tuple) :: non_neg_integer
  def tuple_size(tuple) do
    :erlang.tuple_size(tuple)
  end

  @doc """
  Arithmetic addition.

  Allowed in guard tests. Inlined by the computer.

  ## Examples

      iex> 1 + 2
      3

  """
  @spec (number + number) :: number
  def left + right do
    :erlang.+(left, right)
  end

  @doc """
  Arithmetic subtraction.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      iex> 1 - 2
      -1

  """
  @spec (number - number) :: number
  def left - right do
    :erlang.-(left, right)
  end

  @doc """
  Arithmetic unary plus.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      iex> +1
      1

  """
  @spec (+number) :: number
  def (+value) do
    :erlang.+(value)
  end

  @doc """
  Arithmetic unary minus.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      iex> -2
      -2

  """
  @spec (-number) :: number
  def (-value) do
    :erlang.-(value)
  end

  @doc """
  Arithmetic multiplication.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      iex> 1 * 2
      2

  """
  @spec (number, number) :: number
  def left * right do
    :erlang.*(left, right)
  end

  @doc """
  Arithmetic division.

  The result is always a float. Use `div/2` and `rem/2` if you want
  an integer division or the remainder.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      iex> 1 / 2
      0.5

      iex> 2 / 1
      2.0

  """
  @spec (number / number) :: float
  def left / right do
    :erlang./(left, right)
  end

  @doc """
  Concatenates a proper list and a term, returning a list.

  The complexity of `a ++ b` is proportional to `length(a)`, so avoid repeatedly
  appending to lists of arbitrary length, e.g. `list ++ [item]`.
  Instead, consider prepending via `[item | rest ]` and then reversing.

  If the `right` operand is not a proper list, it returns an improper list.
  If the `left` operand is not a proper list, it raises `ArgumentError`.

  Inlined by the compiler.

  ## Examples

      iex> [1] ++ [2, 3]
      [1, 2, 3]

      iex> 'foo' ++ 'bar'
      'foobar'

      # returns an improper list
      iex> [1] ++ 2
      [1 | 2]

      # returns a proper list
      iex> [1] ++ [2]
      [1, 2]

      # improper list on the right will return an improper list
      iex> [1] ++ [2 | 3]
      [1, 2 | 3]

  """
  @spec (list ++ term) :: maybe_improper_list
  def left ++ right do
    :erlang.++(left, right)
  end

  
end
