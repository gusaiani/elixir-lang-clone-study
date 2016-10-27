defmodule String.Unicode do
  @moduledoc false
  def version, do: {8, 0, 0}

  cluster_path = Path.join(__DIR__, "GraphemeBreakProperty.txt")
  regex = ~r/(?:^([0-9A-F]+)(?:\.\.([0-9A-F]+))?)\s+;\s(\w+)/m

  cluster = Enum.reduce File.stream!(cluster_path), %{}, fn line, dict ->
    [_full, first, last, class] = Regex.run(regex, line)
end
