Code.require_file "test_helper.exs", __DIR__

defmodule IOTest do
  use ExUnit.Case

  doctest IO

  import ExUnit.CaptureIO

  test "read with count" do
    {:ok, file} = File.open(Path.expand('fixtures/file.txt', __DIR__), [:charlist])
    assert 'FOO' == IO.read(file, 3)
    assert File.close(file) == :ok
  end

  test "read with UTF-8 and binary" do
    {:ok, file} = File.open(Path.expand('fixtures/utf8.txt', __DIR__), [:utf8])
    assert "Русский" == IO.read(file, 7)
    assert File.close(file) == :ok
  end

  test "binread" do
    {:ok, file} = File.open(Path.expand('fixtures/utf8.txt', __DIR__))
    assert "Русский" == IO.binread(file, 14)
    assert File.close(file) == :ok
  end

  test "binread all" do
    {:ok, file} = File.open(Path.expand('fixtures/file.bin', __DIR__))
    assert "LF\nCR\rCRLF\r\nLFCR\n\r" == IO.binread(file, :all)
    assert File.close(file) == :ok
  end
end
