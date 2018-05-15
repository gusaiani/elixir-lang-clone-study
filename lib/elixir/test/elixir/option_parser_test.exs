Code.require_file("test_helper.exs", __DIR__)

defmodule OptionParserTest do
  use ExUnit.Case, async: true

  doctest OptionParser

  test "parses --key value option" do
    assert OptionParser.parse(["--source", "form_docs/", "other"], switches: [source: :string]) ==
             {[source: "form_docs/"], ["other"], []}
  end

  test "parses --key=value option" do
    assert OptionParser.parse(["--source=form_docs/", "other"], switches: [source: :string]) ==
             {[source: "form_docs/"], ["other"], []}
  end

  test "parses overrides options by default" do
    assert OptionParser.parse(
             ["--require", "foo", "--require", "bar", "baz"],
             switches: [require: :string]
           ) == {[require: "bar"], ["baz"], []}
  end
