# Use elixir_bootstrap module to be able to bootstrap Kernel.
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
  module. Those functions are called BIFs (built-in internal functions)
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
  Returns an integer or float which is the arithmetical absolute value of `number`.

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

      iex> binary_part("Hello", 5, -3)
      "llo"

  """
  @spec binary_part(binary, pos_integer, integer) :: binary
  def binary_part(binary, start, length) do
    :erlang.binary_part(binary, start, length)
  end

  @doc """
  Returns an integer which is the size in bits of `bitstring`.

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
  use `Integer.floor_div/2` instead.

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

  In case something goes wrong, you can also use `exit/1` with
  a different reason:

      exit(:seems_bad)

  If the exit reason is not `:normal`, all the processes linked to the process
  that exited will crash (unless they are trapping exits).

  ## OTP exits

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
  Returns the head of a list. Raises `ArgumentError` if the list is empty.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      iex> hd([1, 2, 3, 4])
      1

  """
  @spec hd(nonempty_maybe_improper_list(elem, any)) :: elem when elem: term
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
  Returns `true` if `term` is a function; otherwise returns `false`.

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
  Returns `true` if `term` is a PID (process identifier); otherwise returns `false`.

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
  Returns `true` if `term` is a map; otherwise returns `false`.

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
  @spec max(first, second) :: (first | second) when first: term, second: term
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
  @spec min(first, second) :: (first | second) when first: term, second: term
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

      iex> round(-9)
      -9

  """
  @spec round(float) :: integer
  @spec round(value) :: value when value: integer
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
      spawn_monitor(fn -> send current, {self(), 1 + 2} end)

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
  A non-local return from a function.

  Check `Kernel.SpecialForms.try/1` for more information.

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
  @spec tl(nonempty_maybe_improper_list(elem, tail)) ::
        maybe_improper_list(elem, tail) | tail when elem: term, tail: term
  def tl(list) do
    :erlang.tl(list)
  end

  @doc """
  Returns the integer part of `number`.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      iex> trunc(5.4)
      5

      iex> trunc(-5.99)
      -5

      iex> trunc(-5)
      -5

  """
  @spec trunc(value) :: value when value: integer
  @spec trunc(float) :: integer
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

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      iex> 1 + 2
      3

  """
  @spec (integer + integer) :: integer
  @spec (float + float) :: float
  @spec (integer + float) :: float
  @spec (float + integer) :: float
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
  @spec (integer - integer) :: integer
  @spec (float - float) :: float
  @spec (integer - float) :: float
  @spec (float - integer) :: float
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
  @spec (+value) :: value when value: number
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
  @spec (-0) :: 0
  @spec (-pos_integer) :: neg_integer
  @spec (-neg_integer) :: pos_integer
  @spec (-float) :: float
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
  @spec (integer * integer) :: integer
  @spec (float * float) :: float
  @spec (integer * float) :: float
  @spec (float * integer) :: float
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

      iex> -3.0 / 2.0
      -1.5

      iex> 5 / 1
      5.0

  """
  @spec (number / number) :: float
  def left / right do
    :erlang./(left, right)
  end

  @doc """
  Concatenates a proper list and a term, returning a list.

  The complexity of `a ++ b` is proportional to `length(a)`, so avoid repeatedly
  appending to lists of arbitrary length, e.g. `list ++ [item]`.
  Instead, consider prepending via `[item | rest]` and then reversing.

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

  @doc """
  Removes the first occurrence of an item on the left list
  for each item on the right.

  The complexity of `a -- b` is proportional to `length(a) * length(b)`,
  meaning that it will be very slow if both `a` and `b` are long lists.
  In such cases, consider converting each list to a `MapSet` and using
  `MapSet.difference/2`.

  Inlined by the compiler.

  ## Examples

      iex> [1, 2, 3] -- [1, 2]
      [3]

      iex> [1, 2, 3, 2, 1] -- [1, 2, 2]
      [3, 1]

  """
  @spec (list -- list) :: list
  def left -- right do
    :erlang.--(left, right)
  end

  @doc """
  Boolean not.

  `arg` must be a boolean; if it's not, an `ArgumentError` exception is raised.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      iex> not false
      true

  """
  @spec not(true) :: false
  @spec not(false) :: true
  def not(arg) do
    :erlang.not(arg)
  end

  @doc """
  Returns `true` if left is less than right.

  All terms in Elixir can be compared with each other.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      iex> 1 < 2
      true

  """
  @spec (term < term) :: boolean
  def left < right do
    :erlang.<(left, right)
  end

  @doc """
  Returns `true` if left is more than right.

  All terms in Elixir can be compared with each other.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      iex> 1 > 2
      false

  """
  @spec (term > term) :: boolean
  def left > right do
    :erlang.>(left, right)
  end

  @doc """
  Returns `true` if left is less than or equal to right.

  All terms in Elixir can be compared with each other.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      iex> 1 <= 2
      true

  """
  @spec (term <= term) :: boolean
  def left <= right do
    :erlang."=<"(left, right)
  end

  @doc """
  Returns `true` if left is more than or equal to right.

  All terms in Elixir can be compared with each other.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      iex> 1 >= 2
      false

  """
  @spec (term >= term) :: boolean
  def left >= right do
    :erlang.>=(left, right)
  end

  @doc """
  Returns `true` if the two items are equal.

  This operator considers 1 and 1.0 to be equal. For stricter
  semantics, use `===` instead.

  All terms in Elixir can be compared with each other.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      iex> 1 == 2
      false

      iex> 1 == 1.0
      true

  """
  @spec (term == term) :: boolean
  def left == right do
    :erlang.==(left, right)
  end

  @doc """
  Returns `true` if the two items are not equal.

  This operator considers 1 and 1.0 to be equal. For match
  comparison, use `!==` instead.

  All terms in Elixir can be compared with each other.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      iex> 1 != 2
      true

      iex> 1 != 1.0
      false

  """
  @spec (term != term) :: boolean
  def left != right do
    :erlang."/="(left, right)
  end

  @doc """
  Returns `true` if the two items are exactly equal.

  The items are only considered to be exactly equal if they
  have the same value and are of the same type. For example,
  `1 == 1.0` returns true, but since they are of different
  types, `1 === 1.0` returns false.

  All terms in Elixir can be compared with each other.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      iex> 1 === 2
      false

      iex> 1 === 1.0
      false

  """
  @spec (term === term) :: boolean
  def left === right do
    :erlang."=:="(left, right)
  end

  @doc """
  Returns `true` if the two items are not exactly equal.

  All terms in Elixir can be compared with each other.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      iex> 1 !== 2
      true

      iex> 1 !== 1.0
      true

  """
  @spec (term !== term) :: boolean
  def left !== right do
    :erlang."=/="(left, right)
  end

  @doc """
  Gets the element at the zero-based `index` in `tuple`.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      iex> tuple = {:foo, :bar, 3}
      iex> elem(tuple, 1)
      :bar

  """
  @spec elem(tuple, non_neg_integer) :: term
  def elem(tuple, index) do
    :erlang.element(index + 1, tuple)
  end

  @doc """
  Inserts `value` at the given zero-based `index` in `tuple`.

  Inlined by the compiler.

  ## Examples

      iex> tuple = {:foo, :bar, 3}
      iex> put_elem(tuple, 0, :baz)
      {:baz, :bar, 3}

  """
  @spec put_elem(tuple, non_neg_integer, term) :: tuple
  def put_elem(tuple, index, value) do
    :erlang.setelement(index + 1, tuple, value)
  end

  ## Implemented in Elixir

  @doc """
  Boolean or.

  If the first argument is `true`, `true` is returned; otherwise, the second
  argument is returned.

  Requires only the first argument to be a boolean since it short-circuits.
  If the first argument is not a boolean, an `ArgumentError` exception is
  raised.

  Allowed in guard tests.

  ## Examples

      iex> true or false
      true
      iex> false or 42
      42

  """
  defmacro left or right do
    quote do: :erlang.orelse(unquote(left), unquote(right))
  end

  @doc """
  Boolean and.

  If the first argument is `false`, `false` is returned; otherwise, the second
  argument is returned.

  Requires only the first argument to be a boolean since it short-circuits. If
  the first argument is not a boolean, an `ArgumentError` exception is raised.

  Allowed in guard tests.

  ## Examples

      iex> true and false
      false
      iex> true and "yay!"
      "yay!"

  """
  defmacro left and right do
    quote do: :erlang.andalso(unquote(left), unquote(right))
  end

  @doc """
  Boolean not.

  Receives any argument (not just booleans) and returns `true` if the argument
  is `false` or `nil`; returns `false` otherwise.

  Not allowed in guard clauses.

  ## Examples

      iex> !Enum.empty?([])
      false

      iex> !List.first([])
      true

  """
  defmacro !(arg)

  defmacro !({:!, _, [arg]}) do
    optimize_boolean(quote do
      case unquote(arg) do
        x when x in [false, nil] -> false
        _ -> true
      end
    end)
  end

  defmacro !(arg) do
    optimize_boolean(quote do
      case unquote(arg) do
        x when x in [false, nil] -> true
        _ -> false
      end
    end)
  end

  @doc """
  Concatenates two binaries.

  ## Examples

      iex> "foo" <> "bar"
      "foobar"

  The `<>` operator can also be used in pattern matching (and guard clauses) as
  long as the first part is a literal binary:

      iex> "foo" <> x = "foobar"
      iex> x
      "bar"

  `x <> "bar" = "foobar"` would have resulted in a `CompileError` exception.

  """
  defmacro left <> right do
    concats = extract_concatenations({:<>, [], [left, right]})
    quote do: <<unquote_splicing(concats)>>
  end

  # Extracts concatenations in order to optimize many
  # concatenations into one single clause.
  defp extract_concatenations({:<>, _, [left, right]}) do
    [wrap_concatenation(left) | extract_concatenations(right)]
  end

  defp extract_concatenations(other) do
    [wrap_concatenation(other)]
  end

  defp wrap_concatenation(binary) when is_binary(binary) do
    binary
  end

  defp wrap_concatenation(other) do
    {:::, [], [other, {:binary, [], nil}]}
  end

  @doc """
  Raises an exception.

  If the argument `msg` is a binary, it raises a `RuntimeError` exception
  using the given argument as message.

  If `msg` is an atom, it just calls `raise/2` with the atom as the first
  argument and `[]` as the second argument.

  If `msg` is anything else, raises an `ArgumentError` exception.

  ## Examples

      iex> raise "oops"
      ** (RuntimeError) oops

      try do
        1 + :foo
      rescue
        x in [ArithmeticError] ->
          IO.puts "that was expected"
          raise x
      end

  """
  defmacro raise(msg) do
    # Try to figure out the type at compilation time
    # to avoid dead code and make Dialyzer happy.
    msg = case not is_binary(msg) and bootstrapped?(Macro) do
      true  -> Macro.expand(msg, __CALLER__)
      false -> msg
    end

    case msg do
      msg when is_binary(msg) ->
        quote do
          :erlang.error RuntimeError.exception(unquote(msg))
        end
      {:<<>>, _, _} = msg ->
        quote do
          :erlang.error RuntimeError.exception(unquote(msg))
        end
      alias when is_atom(alias) ->
        quote do
          :erlang.error unquote(alias).exception([])
        end
      _ ->
        quote do
          :erlang.error Kernel.Utils.raise(unquote(msg))
        end
    end
  end

  @doc """
  Raises an exception.

  Calls the `exception/1` function on the given argument (which has to be a
  module name like `ArgumentError` or `RuntimeError`) passing `attrs` as the
  attributes in order to retrieve the exception struct.

  Any module that contains a call to the `defexception/1` macro automatically
  implements the `c:Exception.exception/1` callback expected by `raise/2`.
  For more information, see `defexception/1`.

  ## Examples

      iex> raise(ArgumentError, message: "Sample")
      ** (ArgumentError) Sample

  """
  defmacro raise(exception, attrs) do
    quote do
      :erlang.error unquote(exception).exception(unquote(attrs))
    end
  end

  @doc """
  Raises an exception preserving a previous stacktrace.

  Works like `raise/1` but does not generate a new stacktrace.

  Notice that `System.stacktrace/0` returns the stacktrace
  of the last exception. That said, it is common to assign
  the stacktrace as the first expression inside a `rescue`
  clause as any other exception potentially raised (and
  rescued) between the rescue clause and the raise call
  may change the `System.stacktrace/0` value.

  ## Examples

      try do
        raise "oops"
      rescue
        exception ->
          stacktrace = System.stacktrace
          if Exception.message(exception) == "oops" do
            reraise exception, stacktrace
          end
      end
  """
  defmacro reraise(msg, stacktrace) do
    # Try to figure out the type at compilation time
    # to avoid dead code and make Dialyzer happy.

    case Macro.expand(msg, __CALLER__) do
      msg when is_binary(msg) ->
        quote do
          :erlang.raise :error, RuntimeError.exception(unquote(msg)), unquote(stacktrace)
        end
      {:<<>>, _, _} = msg ->
        quote do
          :erlang.raise :error, RuntimeError.exception(unquote(msg)), unquote(stacktrace)
        end
      alias when is_atom(alias) ->
        quote do
          :erlang.raise :error, unquote(alias).exception([]), unquote(stacktrace)
        end
      msg ->
        quote do
          stacktrace = unquote(stacktrace)
          case unquote(msg) do
            msg when is_binary(msg) ->
              :erlang.raise :error, RuntimeError.exception(msg), stacktrace
            atom when is_atom(atom) ->
              :erlang.raise :error, atom.exception([]), stacktrace
            %{__struct__: struct, __exception__: true} = other when is_atom(struct) ->
              :erlang.raise :error, other, stacktrace
            other ->
              message = "reraise/2 expects an alias, string or exception as the first argument, got: #{inspect other}"
              :erlang.error ArgumentError.exception(message)
          end
        end
    end
  end

  @doc """
  Raises an exception preserving a previous stacktrace.

  `reraise/3` works like `reraise/2`, except it passes arguments to the
  `exception/1` function as explained in `raise/2`.

  ## Examples

      try do
        raise "oops"
      rescue
        exception ->
          stacktrace = System.stacktrace
          reraise WrapperError, [exception: exception], stacktrace
      end
  """
  defmacro reraise(exception, attrs, stacktrace) do
    quote do
      :erlang.raise :error, unquote(exception).exception(unquote(attrs)), unquote(stacktrace)
    end
  end

  @doc """
  Matches the term on the left against the regular expression or string on the
  right.

  Returns `true` if `left` matches `right` (if it's a regular expression)
  or contains `right` (if it's a string).

  ## Examples

      iex> "abcd" =~ ~r/c(d)/
      true

      iex> "abcd" =~ ~r/e/
      false

      iex> "abcd" =~ "bc"
      true

      iex> "abcd" =~ "ad"
      false

      iex> "abcd" =~ ""
      true

  """
  @spec (String.t =~ (String.t | Regex.t)) :: boolean
  def left =~ "" when is_binary(left), do: true

  def left =~ right when is_binary(left) and is_binary(right) do
    :binary.match(left, right) != :nomatch
  end

  def left =~ right when is_binary(left) do
    Regex.match?(right, left)
  end

  @doc ~S"""
  Inspects the given argument according to the `Inspect` protocol.
  The second argument is a keyword list with options to control
  inspection.

  ## Options

  `inspect/2` accepts a list of options that are internally
  translated to an `Inspect.Opts` struct. Check the docs for
  `Inspect.Opts` to see the supported options.

  ## Examples

      iex> inspect(:foo)
      ":foo"

      iex> inspect [1, 2, 3, 4, 5], limit: 3
      "[1, 2, 3, ...]"

      iex> inspect [1, 2, 3], pretty: true, width: 0
      "[1,\n 2,\n 3]"

      iex> inspect("olá" <> <<0>>)
      "<<111, 108, 195, 161, 0>>"

      iex> inspect("olá" <> <<0>>, binaries: :as_strings)
      "\"olá\\0\""

      iex> inspect("olá", binaries: :as_binaries)
      "<<111, 108, 195, 161>>"

      iex> inspect('bar')
      "'bar'"

      iex> inspect([0 | 'bar'])
      "[0, 98, 97, 114]"

      iex> inspect(100, base: :octal)
      "0o144"

      iex> inspect(100, base: :hex)
      "0x64"

  Note that the `Inspect` protocol does not necessarily return a valid
  representation of an Elixir term. In such cases, the inspected result
  must start with `#`. For example, inspecting a function will return:

      inspect fn a, b -> a + b end
      #=> #Function<...>

  """
  @spec inspect(Inspect.t, Keyword.t) :: String.t
  def inspect(arg, opts \\ []) when is_list(opts) do
    opts  = struct(Inspect.Opts, opts)
    limit = case opts.pretty do
      true  -> opts.width
      false -> :infinity
    end
    IO.iodata_to_binary(
      Inspect.Algebra.format(Inspect.Algebra.to_doc(arg, opts), limit)
    )
  end

  @doc """
  Creates and updates structs.

  The `struct` argument may be an atom (which defines `defstruct`)
  or a `struct` itself. The second argument is any `Enumerable` that
  emits two-element tuples (key-value pairs) during enumeration.

  Keys in the `Enumerable` that don't exist in the struct are automatically
  discarded. Note that keys must be atoms, as only atoms are allowed when
  defining a struct.

  This function is useful for dynamically creating and updating structs, as
  well as for converting maps to structs; in the latter case, just inserting
  the appropriate `:__struct__` field into the map may not be enough and
  `struct/2` should be used instead.

  ## Examples

      defmodule User do
        defstruct name: "john"
      end

      struct(User)
      #=> %User{name: "john"}

      opts = [name: "meg"]
      user = struct(User, opts)
      #=> %User{name: "meg"}

      struct(user, unknown: "value")
      #=> %User{name: "meg"}

      struct(User, %{name: "meg"})
      #=> %User{name: "meg"}

      # String keys are ignored
      struct(User, %{"name" => "meg"})
      #=> %User{name: "john"}

  """
  @spec struct(module | struct, Enum.t) :: struct
  def struct(struct, kv \\ []) do
    struct(struct, kv, fn({key, val}, acc) ->
      case :maps.is_key(key, acc) and key != :__struct__ do
        true  -> :maps.put(key, val, acc)
        false -> acc
      end
    end)
  end

  @doc """
  Similar to `struct/2` but checks for key validity.

  The function `struct!/2` emulates the compile time behaviour
  of structs. This means that:

    * when building a struct, as in `struct!(SomeStruct, key: :value)`,
      it is equivalent to `%SomeStruct{key: :value}` and therefore this
      function will check if every given key-value belongs to the struct.
      If the struct is enforcing any key via `@enforce_keys`, those will
      be enforced as well;

    * when updating a struct, as in `struct!(%SomeStruct{}, key: :value)`,
      it is equivalent to `%SomeStruct{struct | key: :value}` and therefore this
      function will check if every given key-value belongs to the struct.
      However, updating structs does not enforce keys, as keys are enforced
      only when building;

  """
  @spec struct!(module | struct, Enum.t) :: struct | no_return
  def struct!(struct, kv \\ [])

  def struct!(struct, kv) when is_atom(struct) do
    struct.__struct__(kv)
  end

  def struct!(struct, kv) when is_map(struct) do
    struct(struct, kv, fn
      {:__struct__, _}, acc -> acc
      {key, val}, acc ->
        :maps.update(key, val, acc)
    end)
  end

  defp struct(struct, [], _fun) when is_atom(struct) do
    struct.__struct__()
  end

  defp struct(struct, kv, fun) when is_atom(struct) do
    struct(struct.__struct__(), kv, fun)
  end

  defp struct(%{__struct__: _} = struct, [], _fun) do
    struct
  end

  defp struct(%{__struct__: _} = struct, kv, fun) do
    Enum.reduce(kv, struct, fun)
  end

  @doc """
  Gets a value from a nested structure.

  Uses the `Access` module to traverse the structures
  according to the given `keys`, unless the `key` is a
  function.

  If a key is a function, the function will be invoked
  passing three arguments, the operation (`:get`), the
  data to be accessed, and a function to be invoked next.

  This means `get_in/2` can be extended to provide
  custom lookups. The downside is that functions cannot be
  stored as keys in the accessed data structures.

  ## Examples

      iex> users = %{"john" => %{age: 27}, "meg" => %{age: 23}}
      iex> get_in(users, ["john", :age])
      27

  In case any of entries in the middle returns `nil`, `nil` will be returned
  as per the Access module:

      iex> users = %{"john" => %{age: 27}, "meg" => %{age: 23}}
      iex> get_in(users, ["unknown", :age])
      nil

  When one of the keys is a function, the function is invoked.
  In the example below, we use a function to get all the maps
  inside a list:

      iex> users = [%{name: "john", age: 27}, %{name: "meg", age: 23}]
      iex> all = fn :get, data, next -> Enum.map(data, next) end
      iex> get_in(users, [all, :age])
      [27, 23]

  If the previous value before invoking the function is `nil`,
  the function *will* receive nil as a value and must handle it
  accordingly.
  """
  @spec get_in(Access.t, nonempty_list(term)) :: term
  def get_in(data, keys)

  def get_in(data, [h]) when is_function(h),
    do: h.(:get, data, &(&1))
  def get_in(data, [h | t]) when is_function(h),
    do: h.(:get, data, &get_in(&1, t))

  def get_in(nil, [_]),
    do: nil
  def get_in(nil, [_ | t]),
    do: get_in(nil, t)

  def get_in(data, [h]),
    do: Access.get(data, h)
  def get_in(data, [h | t]),
    do: get_in(Access.get(data, h), t)

  @doc """
  Puts a value in a nested structure.

  Uses the `Access` module to traverse the structures
  according to the given `keys`, unless the `key` is a
  function. If the key is a function, it will be invoked
  as specified in `get_and_update_in/3`.

  ## Examples

      iex> users = %{"john" => %{age: 27}, "meg" => %{age: 23}}
      iex> put_in(users, ["john", :age], 28)
      %{"john" => %{age: 28}, "meg" => %{age: 23}}

  In case any of entries in the middle returns `nil`,
  an error will be raised when trying to access it next.
  """
  @spec put_in(Access.t, nonempty_list(term), term) :: Access.t
  def put_in(data, keys, value) do
    elem(get_and_update_in(data, keys, fn _ -> {nil, value} end), 1)
  end

  @doc """
  Updates a key in a nested structure.

  Uses the `Access` module to traverse the structures
  according to the given `keys`, unless the `key` is a
  function. If the key is a function, it will be invoked
  as specified in `get_and_update_in/3`.

  ## Examples

      iex> users = %{"john" => %{age: 27}, "meg" => %{age: 23}}
      iex> update_in(users, ["john", :age], &(&1 + 1))
      %{"john" => %{age: 28}, "meg" => %{age: 23}}

  In case any of entries in the middle returns `nil`,
  an error will be raised when trying to access it next.
  """
  @spec update_in(Access.t, nonempty_list(term), (term -> term)) :: Access.t
  def update_in(data, keys, fun) when is_function(fun, 1) do
    elem(get_and_update_in(data, keys, fn x -> {nil, fun.(x)} end), 1)
  end

  @doc """
  Gets a value and updates a nested structure.

  It expects a tuple to be returned, containing the value
  retrieved and the update one. The `fun` may also
  return `:pop`, implying the current value shall be removed
  from the structure and returned.

  It uses the `Access` module to traverse the structures
  according to the given `keys`, unless the `key` is a
  function.

  If a key is a function, the function will be invoked
  passing three arguments, the operation (`:get_and_update`),
  the data to be accessed, and a function to be invoked next.

  This means `get_and_update_in/3` can be extended to provide
  custom lookups. The downside is that functions cannot be stored
  as keys in the accessed data structures.

  ## Examples

  This function is useful when there is a need to retrieve the current
  value (or something calculated in function of the current value) and
  update it at the same time. For example, it could be used to increase
  the age of a user by one and return the previous age in one pass:

      iex> users = %{"john" => %{age: 27}, "meg" => %{age: 23}}
      iex> get_and_update_in(users, ["john", :age], &{&1, &1 + 1})
      {27, %{"john" => %{age: 28}, "meg" => %{age: 23}}}

  When one of the keys is a function, the function is invoked.
  In the example below, we use a function to get and increment all
  ages inside a list:

      iex> users = [%{name: "john", age: 27}, %{name: "meg", age: 23}]
      iex> all = fn :get_and_update, data, next ->
      ...>   Enum.map(data, next) |> :lists.unzip
      ...> end
      iex> get_and_update_in(users, [all, :age], &{&1, &1 + 1})
      {[27, 23], [%{name: "john", age: 28}, %{name: "meg", age: 24}]}

  If the previous value before invoking the function is `nil`,
  the function *will* receive `nil` as a value and must handle it
  accordingly (be it by failing or providing a sane default).

  The `Access` module ships with many convenience accessor functions,
  like the `all` anonymous function defined above. See `Access.all/0`,
  `Access.key/2` and others as examples.
  """
  @spec get_and_update_in(Access.t, nonempty_list(term), (term -> {get, term} | :pop)) ::
                          {get, Access.t} when get: var
  def get_and_update_in(data, keys, fun)

  def get_and_update_in(data, [h], fun) when is_function(h),
    do: h.(:get_and_update, data, fun)
  def get_and_update_in(data, [h | t], fun) when is_function(h),
    do: h.(:get_and_update, data, &get_and_update_in(&1, t, fun))

  def get_and_update_in(data, [h], fun),
    do: Access.get_and_update(data, h, fun)
  def get_and_update_in(data, [h | t], fun),
    do: Access.get_and_update(data, h, &get_and_update_in(&1, t, fun))

  @doc """
  Pops a key from the given nested structure.

  Uses the `Access` protocol to traverse the structures
  according to the given `keys`, unless the `key` is a
  function. If the key is a function, it will be invoked
  as specified in `get_and_update_in/3`.

  ## Examples

      iex> users = %{"john" => %{age: 27}, "meg" => %{age: 23}}
      iex> pop_in(users, ["john", :age])
      {27, %{"john" => %{}, "meg" => %{age: 23}}}

  In case any entry returns `nil`, its key will be removed
  and the deletion will be considered a success.
  """
  @spec pop_in(Access.t, nonempty_list(term)) :: {term, Access.t}
  def pop_in(data, keys)
  def pop_in(nil, [h | _]), do: Access.pop(nil, h)
  def pop_in(data, keys), do: do_pop_in(data, keys)

  defp do_pop_in(nil, [_ | _]),
    do: :pop
  defp do_pop_in(data, [h]) when is_function(h),
    do: h.(:get_and_update, data, fn _ -> :pop end)
  defp do_pop_in(data, [h | t]) when is_function(h),
    do: h.(:get_and_update, data, &do_pop_in(&1, t))
  defp do_pop_in(data, [h]),
    do: Access.pop(data, h)
  defp do_pop_in(data, [h | t]),
    do: Access.get_and_update(data, h, &do_pop_in(&1, t))

  @doc """
  Puts a value in a nested structure via the given `path`.

  This is similar to `put_in/3`, except the path is extracted via
  a macro rather than passing a list. For example:

      put_in(opts[:foo][:bar], :baz)

  Is equivalent to:

      put_in(opts, [:foo, :bar], :baz)

  Note that in order for this macro to work, the complete path must always
  be visible by this macro. For more information about the supported path
  expressions, please check `get_and_update_in/2` docs.

  ## Examples

      iex> users = %{"john" => %{age: 27}, "meg" => %{age: 23}}
      iex> put_in(users["john"][:age], 28)
      %{"john" => %{age: 28}, "meg" => %{age: 23}}

      iex> users = %{"john" => %{age: 27}, "meg" => %{age: 23}}
      iex> put_in(users["john"].age, 28)
      %{"john" => %{age: 28}, "meg" => %{age: 23}}

  """
  defmacro put_in(path, value) do
    case unnest(path, [], true, "put_in/2") do
      {[h | t], true} ->
        nest_update_in(h, t, quote(do: fn _ -> unquote(value) end))
      {[h | t], false} ->
        expr = nest_get_and_update_in(h, t, quote(do: fn _ -> {nil, unquote(value)} end))
        quote do: :erlang.element(2, unquote(expr))
    end
  end

  @doc """
  Pops a key from the nested structure via the given `path`.

  This is similar to `pop_in/2`, except the path is extracted via
  a macro rather than passing a list. For example:

      pop_in(opts[:foo][:bar])

  Is equivalent to:

      pop_in(opts, [:foo, :bar])

  Note that in order for this macro to work, the complete path must always
  be visible by this macro. For more information about the supported path
  expressions, please check `get_and_update_in/2` docs.

  ## Examples

      iex> users = %{"john" => %{age: 27}, "meg" => %{age: 23}}
      iex> pop_in(users["john"][:age])
      {27, %{"john" => %{}, "meg" => %{age: 23}}}

      iex> users = %{john: %{age: 27}, meg: %{age: 23}}
      iex> pop_in(users.john[:age])
      {27, %{john: %{}, meg: %{age: 23}}}

  In case any entry returns `nil`, its key will be removed
  and the deletion will be considered a success.
  """
  defmacro pop_in(path) do
    {[h | t], _} = unnest(path, [], true, "pop_in/1")
    nest_pop_in(:map, h, t)
  end

  @doc """
  Updates a nested structure via the given `path`.

  This is similar to `update_in/3`, except the path is extracted via
  a macro rather than passing a list. For example:

      update_in(opts[:foo][:bar], &(&1 + 1))

  Is equivalent to:

      update_in(opts, [:foo, :bar], &(&1 + 1))

  Note that in order for this macro to work, the complete path must always
  be visible by this macro. For more information about the supported path
  expressions, please check `get_and_update_in/2` docs.

  ## Examples

      iex> users = %{"john" => %{age: 27}, "meg" => %{age: 23}}
      iex> update_in(users["john"][:age], &(&1 + 1))
      %{"john" => %{age: 28}, "meg" => %{age: 23}}

      iex> users = %{"john" => %{age: 27}, "meg" => %{age: 23}}
      iex> update_in(users["john"].age, &(&1 + 1))
      %{"john" => %{age: 28}, "meg" => %{age: 23}}

  """
  defmacro update_in(path, fun) do
    case unnest(path, [], true, "update_in/2") do
      {[h | t], true} ->
        nest_update_in(h, t, fun)
      {[h | t], false} ->
        expr = nest_get_and_update_in(h, t, quote(do: fn x -> {nil, unquote(fun).(x)} end))
        quote do: :erlang.element(2, unquote(expr))
    end
  end

  @doc """
  Gets a value and updates a nested data structure via the given `path`.

  This is similar to `get_and_update_in/3`, except the path is extracted
  via a macro rather than passing a list. For example:

      get_and_update_in(opts[:foo][:bar], &{&1, &1 + 1})

  Is equivalent to:

      get_and_update_in(opts, [:foo, :bar], &{&1, &1 + 1})

  Note that in order for this macro to work, the complete path must always
  be visible by this macro. See the Paths section below.

  ## Examples

      iex> users = %{"john" => %{age: 27}, "meg" => %{age: 23}}
      iex> get_and_update_in(users["john"].age, &{&1, &1 + 1})
      {27, %{"john" => %{age: 28}, "meg" => %{age: 23}}}

  ## Paths

  A path may start with a variable, local or remote call, and must be
  followed by one or more:

    * `foo[bar]` - accesses the key `bar` in `foo`; in case `foo` is nil,
      `nil` is returned

    * `foo.bar` - accesses a map/struct field; in case the field is not
      present, an error is raised

  Here are some valid paths:

      users["john"][:age]
      users["john"].age
      User.all["john"].age
      all_users()["john"].age

  Here are some invalid ones:

      # Does a remote call after the initial value
      users["john"].do_something(arg1, arg2)

      # Does not access any key or field
      users

  """
  defmacro get_and_update_in(path, fun) do
    {[h | t], _} = unnest(path, [], true, "get_and_update_in/2")
    nest_get_and_update_in(h, t, fun)
  end

  defp nest_update_in([], fun),  do: fun
  defp nest_update_in(list, fun) do
    quote do
      fn x -> unquote(nest_update_in(quote(do: x), list, fun)) end
    end
  end
  defp nest_update_in(h, [{:map, key} | t], fun) do
    quote do
      Map.update!(unquote(h), unquote(key), unquote(nest_update_in(t, fun)))
    end
  end

  defp nest_get_and_update_in([], fun),  do: fun
  defp nest_get_and_update_in(list, fun) do
    quote do
      fn x -> unquote(nest_get_and_update_in(quote(do: x), list, fun)) end
    end
  end
  defp nest_get_and_update_in(h, [{:access, key} | t], fun) do
    quote do
      Access.get_and_update(
        unquote(h),
        unquote(key),
        unquote(nest_get_and_update_in(t, fun))
      )
    end
  end
  defp nest_get_and_update_in(h, [{:map, key} | t], fun) do
    quote do
      Map.get_and_update!(unquote(h), unquote(key), unquote(nest_get_and_update_in(t, fun)))
    end
  end

  defp nest_pop_in(kind, list) do
    quote do
      fn x -> unquote(nest_pop_in(kind, quote(do: x), list)) end
    end
  end

  defp nest_pop_in(:map, h, [{:access, key}]) do
    quote do
      case unquote(h) do
        nil -> {nil, nil}
        h   -> Access.pop(h, unquote(key))
      end
    end
  end

  defp nest_pop_in(_, _, [{:map, key}]) do
    raise ArgumentError, "cannot use pop_in when the last segment is a map/struct field. " <>
                         "This would effectively remove the field #{inspect key} from the map/struct"
  end
  defp nest_pop_in(_, h, [{:map, key} | t]) do
    quote do
      Map.get_and_update!(unquote(h), unquote(key), unquote(nest_pop_in(:map, t)))
    end
  end

  defp nest_pop_in(_, h, [{:access, key}]) do
    quote do
      case unquote(h) do
        nil -> :pop
        h   -> Access.pop(h, unquote(key))
      end
    end
  end
  defp nest_pop_in(_, h, [{:access, key} | t]) do
    quote do
      Access.get_and_update(unquote(h), unquote(key), unquote(nest_pop_in(:access, t)))
    end
  end

  defp unnest({{:., _, [Access, :get]}, _, [expr, key]}, acc, _all_map?, kind) do
    unnest(expr, [{:access, key} | acc], false, kind)
  end

  defp unnest({{:., _, [expr, key]}, _, []}, acc, all_map?, kind)
      when is_tuple(expr) and
           :erlang.element(1, expr) != :__aliases__ and
           :erlang.element(1, expr) != :__MODULE__ do
    unnest(expr, [{:map, key} | acc], all_map?, kind)
  end

  defp unnest(other, [], _all_map?, kind) do
    raise ArgumentError,
      "expected expression given to #{kind} to access at least one element, got: #{Macro.to_string other}"
  end

  defp unnest(other, acc, all_map?, kind) do
    case proper_start?(other) do
      true -> {[other | acc], all_map?}
      false ->
        raise ArgumentError,
          "expression given to #{kind} must start with a variable, local or remote call " <>
          "and be followed by an element access, got: #{Macro.to_string other}"
    end
  end

  defp proper_start?({{:., _, [expr, _]}, _, _args})
    when is_atom(expr)
    when :erlang.element(1, expr) == :__aliases__
    when :erlang.element(1, expr) == :__MODULE__, do: true

  defp proper_start?({atom, _, _args})
    when is_atom(atom), do: true

  defp proper_start?(other),
    do: not is_tuple(other)

  @doc """
  Converts the argument to a string according to the
  `String.Chars` protocol.

  This is the function invoked when there is string interpolation.

  ## Examples

      iex> to_string(:foo)
      "foo"

  """
  defmacro to_string(arg) do
    quote do: String.Chars.to_string(unquote(arg))
  end

  @doc """
  Converts the argument to a charlist according to the `List.Chars` protocol.

  ## Examples

      iex> to_charlist(:foo)
      'foo'

  """
  defmacro to_charlist(arg) do
    quote do: List.Chars.to_charlist(unquote(arg))
  end

  @doc """
  Returns `true` if `term` is `nil`, `false` otherwise.

  Allowed in guard clauses.

  ## Examples

      iex> is_nil(1)
      false

      iex> is_nil(nil)
      true

  """
  defmacro is_nil(term) do
    quote do: unquote(term) == nil
  end

  @doc """
  A convenience macro that checks if the right side (an expression) matches the
  left side (a pattern).

  ## Examples

      iex> match?(1, 1)
      true

      iex> match?(1, 2)
      false

      iex> match?({1, _}, {1, 2})
      true

      iex> map = %{a: 1, b: 2}
      iex> match?(%{a: _}, map)
      true

      iex> a = 1
      iex> match?(^a, 1)
      true

  `match?/2` is very useful when filtering of finding a value in an enumerable:

      list = [{:a, 1}, {:b, 2}, {:a, 3}]
      Enum.filter list, &match?({:a, _}, &1)
      #=> [{:a, 1}, {:a, 3}]

  Guard clauses can also be given to the match:

      list = [{:a, 1}, {:b, 2}, {:a, 3}]
      Enum.filter list, &match?({:a, x} when x < 2, &1)
      #=> [{:a, 1}]

  However, variables assigned in the match will not be available
  outside of the function call (unlike regular pattern matching with the `=`
  operator):

      iex> match?(_x, 1)
      true
      iex> binding()
      []

  """
  defmacro match?(pattern, expr) do
    quote do
      case unquote(expr) do
        unquote(pattern) ->
          true
        _ ->
          false
      end
    end
  end

  @doc """
  Reads and writes attributes of the current module.

  The canonical example for attributes is annotating that a module
  implements the OTP behaviour called `gen_server`:

      defmodule MyServer do
        @behaviour :gen_server
        # ... callbacks ...
      end

  By default Elixir supports all the module attributes supported by Erlang, but
  custom attributes can be used as well:

      defmodule MyServer do
        @my_data 13
        IO.inspect @my_data #=> 13
      end

  Unlike Erlang, such attributes are not stored in the module by default since
  it is common in Elixir to use custom attributes to store temporary data that
  will be available at compile-time. Custom attributes may be configured to
  behave closer to Erlang by using `Module.register_attribute/3`.

  Finally, notice that attributes can also be read inside functions:

      defmodule MyServer do
        @my_data 11
        def first_data, do: @my_data
        @my_data 13
        def second_data, do: @my_data
      end

      MyServer.first_data #=> 11
      MyServer.second_data #=> 13

  It is important to note that reading an attribute takes a snapshot of
  its current value. In other words, the value is read at compilation
  time and not at runtime. Check the `Module` module for other functions
  to manipulate module attributes.
  """
  defmacro @(expr)

  defmacro @({name, _, args}) do
    # Check for Module as it is compiled later than Kernel
    case bootstrapped?(Module) do
      false -> nil
      true  ->
        assert_module_scope(__CALLER__, :@, 1)
        function? = __CALLER__.function != nil

        case not function? and __CALLER__.context == :match do
          false -> nil
          true  ->
            raise ArgumentError, "invalid write attribute syntax, you probably meant to use: @#{name} expression"
        end

        # Typespecs attributes are special cased by the compiler so far
        case is_list(args) and length(args) == 1 and typespec(name) do
          false ->
            do_at(args, name, function?, __CALLER__)
          macro ->
            case bootstrapped?(Kernel.Typespec) do
              false -> nil
              true  -> quote do: Kernel.Typespec.unquote(macro)(unquote(hd(args)))
            end
        end
    end
  end

  # @attribute(value)
  defp do_at([arg], name, function?, env) do
    case function? do
      true ->
        raise ArgumentError, "cannot set attribute @#{name} inside function/macro"
      false ->
        cond do
          name == :behavior ->
            :elixir_errors.warn env.line, env.file,
                                "@behavior attribute is not supported, please use @behaviour instead"
          :lists.member(name, [:moduledoc, :typedoc, :doc]) ->
            {stack, _} = :elixir_quote.escape(env_stacktrace(env), false)
            arg = {env.line, arg}
            quote do: Module.put_attribute(__MODULE__, unquote(name), unquote(arg), unquote(stack))
          true ->
            quote do: Module.put_attribute(__MODULE__, unquote(name), unquote(arg))
        end
    end
  end

  # @attribute or @attribute()
  defp do_at(args, name, function?, env) when is_atom(args) or args == [] do
    stack = env_stacktrace(env)

    doc_attr? = :lists.member(name, [:moduledoc, :typedoc, :doc])
    case function? do
      true ->
        value =
          with {_, doc} when doc_attr? <- Module.get_attribute(env.module, name, stack),
            do: doc
        try do
          :elixir_quote.escape(value, false)
        rescue
          ex in [ArgumentError] ->
            raise ArgumentError, "cannot inject attribute @#{name} into function/macro because " <> Exception.message(ex)
        else
          {val, _} -> val
        end
      false ->
        {escaped, _} = :elixir_quote.escape(stack, false)
        quote do
          with {_, doc} when unquote(doc_attr?) <- Module.get_attribute(__MODULE__, unquote(name), unquote(escaped)),
            do: doc
        end
    end
  end

  # All other cases
  defp do_at(args, name, _function?, _env) do
    raise ArgumentError, "expected 0 or 1 argument for @#{name}, got: #{length(args)}"
  end

  defp typespec(:type),               do: :deftype
  defp typespec(:typep),              do: :deftypep
  defp typespec(:opaque),             do: :defopaque
  defp typespec(:spec),               do: :defspec
  defp typespec(:callback),           do: :defcallback
  defp typespec(:macrocallback),      do: :defmacrocallback
  defp typespec(:optional_callbacks), do: :defoptional_callbacks
  defp typespec(_),                   do: false

  @doc """
  Returns the binding for the given context as a keyword list.

  In the returned result, keys are variable names and values are the
  corresponding variable values.

  If the given `context` is `nil` (by default it is), the binding for the
  current context is returned.

  ## Examples

      iex> x = 1
      iex> binding()
      [x: 1]
      iex> x = 2
      iex> binding()
      [x: 2]

      iex> binding(:foo)
      []
      iex> var!(x, :foo) = 1
      1
      iex> binding(:foo)
      [x: 1]

  """
  defmacro binding(context \\ nil) do
    in_match? = Macro.Env.in_match?(__CALLER__)
    for {v, c} <- __CALLER__.vars, c == context do
      {v, wrap_binding(in_match?, {v, [generated: true], c})}
    end
  end

  defp wrap_binding(true, var) do
    quote do: ^(unquote(var))
  end

  defp wrap_binding(_, var) do
    var
  end

  @doc """
  Provides an `if/2` macro.

  This macro expects the first argument to be a condition and the second
  argument to be a keyword list.

  ## One-liner examples

      if(foo, do: bar)

  In the example above, `bar` will be returned if `foo` evaluates to
  `true` (i.e., it is neither `false` nor `nil`). Otherwise, `nil` will be
  returned.

  An `else` option can be given to specify the opposite:

      if(foo, do: bar, else: baz)

  ## Blocks examples

  It's also possible to pass a block to the `if/2` macro. The first
  example above would be translated to:

      if foo do
        bar
      end

  Note that `do/end` become delimiters. The second example would
  translate to:

      if foo do
        bar
      else
        baz
      end

  In order to compare more than two clauses, the `cond/1` macro has to be used.
  """
  defmacro if(condition, clauses) do
    build_if(condition, clauses)
  end

  defp build_if(condition, do: do_clause) do
    build_if(condition, do: do_clause, else: nil)
  end

  defp build_if(condition, do: do_clause, else: else_clause) do
    optimize_boolean(quote do
      case unquote(condition) do
        x when x in [false, nil] -> unquote(else_clause)
        _ -> unquote(do_clause)
      end
    end)
  end

  defp build_if(_condition, _arguments) do
    raise(ArgumentError, "invalid or duplicate keys for if, only \"do\" " <>
      "and an optional \"else\" are permitted")
  end

  @doc """
  Provides an `unless` macro.

  This macro evaluates and returns the `do` block passed in as the second
  argument unless `clause` evaluates to `true`. Otherwise, it returns the value
  of the `else` block if present or `nil` if not.

  See also `if/2`.

  ## Examples

      iex> unless(Enum.empty?([]), do: "Hello")
      nil

      iex> unless(Enum.empty?([1, 2, 3]), do: "Hello")
      "Hello"

      iex> unless Enum.sum([2, 2]) == 5 do
      ...>   "Math still works"
      ...> else
      ...>   "Math is broken"
      ...> end
      "Math still works"

  """
  defmacro unless(condition, clauses) do
    build_unless(condition, clauses)
  end

  defp build_unless(condition, do: do_clause) do
    build_unless(condition, do: do_clause, else: nil)
  end

  defp build_unless(condition, do: do_clause, else: else_clause) do
    quote do
      if(unquote(condition), do: unquote(else_clause), else: unquote(do_clause))
    end
  end

  defp build_unless(_condition, _arguments) do
    raise(ArgumentError, "invalid or duplicate keys for unless, only \"do\" " <>
      "and an optional \"else\" are permitted")
  end

  @doc """
  Destructures two lists, assigning each term in the
  right one to the matching term in the left one.

  Unlike pattern matching via `=`, if the sizes of the left
  and right lists don't match, destructuring simply stops
  instead of raising an error.

  ## Examples

      iex> destructure([x, y, z], [1, 2, 3, 4, 5])
      iex> {x, y, z}
      {1, 2, 3}

  In the example above, even though the right list has more entries than the
  left one, destructuring works fine. If the right list is smaller, the
  remaining items are simply set to `nil`:

      iex> destructure([x, y, z], [1])
      iex> {x, y, z}
      {1, nil, nil}

  The left-hand side supports any expression you would use
  on the left-hand side of a match:

      x = 1
      destructure([^x, y, z], [1, 2, 3])

  The example above will only work if `x` matches the first value in the right
  list. Otherwise, it will raise a `MatchError` (like the `=` operator would
  do).
  """
  defmacro destructure(left, right) when is_list(left) do
    quote do
      unquote(left) =
        Kernel.Utils.destructure(unquote(right), unquote(length(left)))
    end
  end

  ## Shared functions

  defp optimize_boolean({:case, meta, args}) do
    {:case, [{:optimize_boolean, true} | meta], args}
  end

  defp env_stacktrace(env) do
    case bootstrapped?(Path) do
      true  -> Macro.Env.stacktrace(env)
      false -> []
    end
  end

  # TODO: Deprecate by v1.5
  @doc false
  defmacro to_char_list(arg) do
    quote do: Kernel.to_charlist(unquote(arg))
  end
end
