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

  test "parses multi-word option" do
    config = [switches: [hello_world: :boolean]]
    assert OptionParser.next(["--hello-world"], config) == {:ok, :hello_world, true, []}
    assert OptionParser.next(["--no-hello-world"], config) == {:ok, :hello_world, false, []}

    assert OptionParser.next(["--no-hello-world"], strict: []) ==
             {:undefined, "--no-hello-world", nil, []}

    assert OptionParser.next(["--no-hello_world"], strict: []) ==
             {:undefined, "--no-hello_world", nil, []}

    config = [strict: [hello_world: :boolean]]
    assert OptionParser.next(["--hello-world"], config) == {:ok, :hello_world, true, []}
    assert OptionParser.next(["--no-hello-world"], config) == {:ok, :hello_world, false, []}
    assert OptionParser.next(["--hello_world"], config) == {:undefined, "--hello_world", nil, []}

    assert OptionParser.next(["--no-hello_world"], config) ==
             {:undefined, "--no-hello_world", nil, []}
  end

  test "parses more than one key/value options using switches" do
    opts = [switches: [source: :string, docs: :string]]

    assert OptionParser.parse(["--source", "from_docs/", "--docs", "show"], opts) ==
             {[source: "from_docs/", docs: "show"], [], []}

    assert OptionParser.parse(["--source", "from_docs/", "--doc", "show"], opts) ==
             {[source: "from_docs/", doc: "show"], [], []}

    assert OptionParser.parse(["--source", "from_docs/", "--doc=show"], opts) ==
             {[source: "from_docs/", doc: "show"], [], []}

    assert OptionParser.parse(["--no-bool"], strict: []) == {[], [], [{"--no-bool", nil}]}
  end

  test "parses more than one key/value options using strict" do
    opts = [strict: [source: :string, docs: :string]]

    assert OptionParser.parse(["--source", "from_docs/", "--docs", "show"], opts) ==
             {[source: "from_docs/", docs: "show"], [], []}

    assert OptionParser.parse(["--source", "from_docs/", "--doc", "show"], opts) ==
             {[source: "from_docs/"], ["show"], [{"--doc", nil}]}

    assert OptionParser.parse(["--source", "from_docs/", "--doc=show"], opts) ==
             {[source: "from_docs/"], [], [{"--doc", nil}]}

    assert OptionParser.parse(["--no-bool"], strict: []) == {[], [], [{"--no-bool", nil}]}
  end

  test "collects multiple invalid options" do
    argv = ["--bad", "opt", "foo", "-o", "bad", "bar"]

    assert OptionParser.parse(argv, switches: [bad: :integer]) ==
             {[], ["foo", "bar"], [{"--bad", "opt"}]}
  end

  test "parse/2 raises when using both options: switches and strict" do
    assert_raise ArgumentError, ":switches and :strict cannot be given together", fn ->
      OptionParser.parse(["--elixir"], switches: [ex: :string], strict: [elixir: :string])
    end
  end
