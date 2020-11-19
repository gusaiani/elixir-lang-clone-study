defmodule ExUnit.Runner do
  @moduledoc false

  def run(opts, load_us) when (is_integer(load_us) or is_nil(load_us)) and is_list(opts) do
    opts = normalize_opts
  end

  defp normalize_opts(opts) do
    {include, exclude} = ExUnit.Filters.normalize
  end
end
