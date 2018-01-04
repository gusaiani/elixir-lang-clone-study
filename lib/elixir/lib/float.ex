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
      iex> Float.parse("56.5xyz")
      {56.5, "xyz"}

      iex> Float.parse("pi")
      :error

  """
  @spec parse(binary) :: {float, binary} | :error
  def parse("-" <> binary) do
    case parse_unsigned(binary) do
      :error -> :error
      {number, remainder} -> {-number, remainder}
    end
  end

  def parse("+" <> binary) do
    parse_unsigned(binary)
  end

  def parse(binary) do
    parse_unsigned(binary)
  end

  defp parse_unsigned(<<digit, rest::binary>>) when digit in ?0..?9,
    do: parse_unsigned(rest, false, false, <<digit>>)

  defp parse_unsigned(binary) when is_binary(binary), do: :error

  defp parse_unsigned(<<digit, rest::binary>>, dot?, e?, acc) when digit in ?0..?9,
    do: parse_unsigned(rest, dot?, e?, <<acc::binary, digit>>)

  defp parse_unsigned(<<?., digit, rest::binary>>, false, false, acc) when digit in ?0..?9,
    do: parse_unsigned(rest, true, false, <<acc::binary, ?., digit>>)

  defp parse_unsigned(<<exp_marker, digit, rest::binary>>, dot?, false, acc)
       when exp_marker in 'eE' and digit in ?0..?9,
       do: parse_unsigned(rest, true, true, <<add_dot(acc, dot?)::binary, ?e, digit>>)

  defp parse_unsigned(<<exp_marker, sign, digit, rest::binary>>, dot?, false, acc)
       when exp_marker in 'eE' and sign in '-+' and digit in ?0..?9,
       do: parse_unsigned(rest, true, true, <<add_dot(acc, dot?)::binary, ?e, sign, digit>>)

  defp parse_unsigned(rest, dot?, _e?, acc),
    do: {:erlang.binary_to_float(add_dot(acc, dot?)), rest}

  defp add_dot(acc, true), do: acc
  defp add_dot(acc, false), do: acc <> ".0"

  @doc """
  Rounds a float to the largest integer less than or equal to `num`.

  `floor/2` also accepts a precision to round a floating-point value down
  to an arbitrary number of fractional digits (between 0 and 15).
  The operation is performed on the binary floating point, without a
  conversion to decimal.

  The behaviour of `floor/2` for floats can be surprising. For example:

      iex> Float.floor(12.52, 2)
      12.51

  One may have expected it to floor to 12.52. This is not a bug.
  Most decimal fractions cannot be represented as a binary floating point
  and therefore the number above is internally represented as 12.51999999,
  which explains the behaviour above.

  This function always returns a float. `Kernel.trunc/1` may be used instead to
  truncate the result to an integer afterwards.

  ## Examples

      iex> Float.floor(34.25)
      34.0
      iex> Float.floor(-56.5)
      -57.0
      iex> Float.floor(34.259, 2)
      34.25

  """
  @spec floor(float, precision_range) :: float
  def floor(number, precision \\ 0)

  def floor(number, precision) when is_float(number) and precision in @precision_range do
    round(number, precision, :floor)
  end

  def floor(number, precision) when is_float(number) do
    raise ArgumentError, invalid_precision_message(precision)
  end

  @doc """
  Rounds a float to the smallest integer greater than or equal to `num`.

  `ceil/2` also accepts a precision to round a floating-point value down
  to an arbitrary number of fractional digits (between 0 and 15).

  The operation is performed on the binary floating point, without a
  conversion to decimal.

  The behaviour of `ceil/2` for floats can be surprising. For example:

      iex> Float.ceil(-12.52, 2)
      -12.51

  One may have expected it to ceil to -12.52. This is not a bug.
  Most decimal fractions cannot be represented as a binary floating point
  and therefore the number above is internally represented as -12.51999999,
  which explains the behaviour above.

  This function always returns floats. `Kernel.trunc/1` may be used instead to
  truncate the result to an integer afterwards.

  ## Examples

      iex> Float.ceil(34.25)
      35.0
      iex> Float.ceil(-56.5)
      -56.0
      iex> Float.ceil(34.251, 2)
      34.26

  """
  @spec ceil(float, precision_range) :: float
  def ceil(number, precision \\ 0)

  def ceil(number, precision) when is_float(number) and precision in @precision_range do
    round(number, precision, :ceil)
  end

  def ceil(number, precision) when is_float(number) do
    raise ArgumentError, invalid_precision_message(precision)
  end

  @doc """
  Rounds a floating-point value to an arbitrary number of fractional
  digits (between 0 and 15).

  The rounding direction always ties to half up. The operation is
  performed on the binary floating point, without a conversion to decimal.

  This function only accepts floats and always returns a float. Use
  `Kernel.round/1` if you want a function that accepts both floats
  and integers and always returns an integer.

  The behaviour of `round/2` for floats can be surprising. For example:

      iex> Float.round(5.5675, 3)
      5.567

  One may have expected it to round to the half up 5.568. This is not a bug.
  Most decimal fractions cannot be represented as a binary floating point
  and therefore the number above is internally represented as 5.567499999,
  which explains the behaviour above. If you want exact rounding for decimals,
  you must use a decimal library. The behaviour above is also in accordance
  to reference implementations, such as "Correctly Rounded Binary-Decimal and
  Decimal-Binary Conversions" by David M. Gay.

  ## Examples

      iex> Float.round(12.5)
      13.0
      iex> Float.round(5.5674, 3)
      5.567
      iex> Float.round(5.5675, 3)
      5.567
      iex> Float.round(-5.5674, 3)
      -5.567
      iex> Float.round(-5.5675)
      -6.0
      iex> Float.round(12.341444444444441, 15)
      12.341444444444441

  """
  @spec round(float, precision_range) :: float
  # This implementation is slow since it relies on big integers.
  # Faster implementations are available on more recent papers
  # and could be implemented in the future.
  def round(float, precision \\ 0)

  def round(float, precision) when is_float(float) and precision in @precision_range do
    round(float, precision, :half_up)
  end

  def round(number, precision) when is_float(number) do
    raise ArgumentError, invalid_precision_message(precision)
  end

  defp round(float, precision, rounding) do
    <<sign::1, exp::11, significant::52-bitstring>> = <<float::float>>
    {num, count, _} = decompose(significant)
    count = count - exp + 1023

    cond do
      # There is no decimal precision
      # zero or minus zero
      count <= 0 or (0 == exp and <<0::52>> == significant) ->
        float

      # Precision beyond 15 digits
      count >= 104 ->
        case rounding do
          :ceil when sign === 0 -> 1 / power_of_10(precision)
          :floor when sign === 1 -> -1 / power_of_10(precision)
          _ -> 0.0
        end

      # We are asking more precision than we have
      count <= precision ->
        float

      true ->
        # Difference in precision between float and asked precision
        # We subtract 1 because we need to calculate the remainder too
        diff = count - precision - 1

        # Get up to latest so we calculate the remainder
        power_of_10 = power_of_10(diff)

        # Convert the numerand to decimal base
        num = num * power_of_5(count)

        # Move to the given precision - 1
        num = div(num, power_of_10)
        div = div(num, 10)
        num = rounding(rounding, sign, num, div)

        # Convert back to float without loss
        # http://www.exploringbinary.com/correct-decimal-to-floating-point-using-big-integers/
        den = power_of_10(precision)
        boundary = den <<< 52

        cond do
          num == 0 ->
            0.0

          num >= boundary ->
            {den, exp} = scale_down(num, boundary, 52)
            decimal_to_float(sign, num, den, exp)

          true ->
            {num, exp} = scale_up(num, boundary, 52)
            decimal_to_float(sign, num, den, exp)
        end
    end
  end

  defp scale_up(num, boundary, exp) when num >= boundary, do: {num, exp}
  defp scale_up(num, boundary, exp), do: scale_up(num <<< 1, boundary, exp - 1)

  defp scale_down(num, den, exp) do
    new_den = den <<< 1

    if num < new_den do
      {den >>> 52, exp}
    else
      scale_down(num, new_den, exp + 1)
    end
  end

  defp decimal_to_float(sign, num, den, exp) do
    quo = div(num, den)
    rem = num - quo * den

    tmp =
      case den >>> 1 do
        den when rem > den -> quo + 1
        den when rem < den -> quo
        _ when (quo &&& 1) === 1 -> quo + 1
        _ -> quo
      end

    tmp = tmp - @power_of_2_to_52
    <<tmp::float>> = <<sign::1, exp + 1023::11, tmp::52>>
    tmp
  end
end
