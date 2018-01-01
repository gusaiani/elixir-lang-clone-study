import Kernel, except: [round: 1]

defmodule Float do
  @moduledoc """
  Functions for working with floating-point numbers.
  """

  import Bitwise

  @power_of_2_to_52 4_503_599_627_370_496
  @precision_range 0..15
  @type precision_range :: 0..15

  @doc """
  Parses a binary into a float.

  If successful, returns a tuple in the form of `{float, remainder_of_binary}`;
  when the binary cannot be coerced into a valid float, the atom `:error` is
  returned.

  If the size of float exceeds the maximum size of `1.7976931348623157e+308`,
  the `ArgumentError` exception is raised.

  If you want to convert a string-formatted float directly to a float,
  `String.to_float/1` can be used instead.

  ## Examples

      iex> Float.parse("34")
      {34.0, ""}
      iex> Float.parse("34.25")
      {34.25, ""}


  """
