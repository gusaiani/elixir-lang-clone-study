Code.require_file "test_helper.exs", __DIR__

defmodule CodeTest do
  use ExUnit.Case, async: true

  doctest Code

  import PathHelpers

  def genmodule(name) do
    defmodule name do
      Kernel.LexicalTracker.remote_references(__MODULE__)
