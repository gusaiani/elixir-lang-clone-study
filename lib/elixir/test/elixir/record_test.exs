Code.require_file("test_helper.exs", __DIR__)

defmodule RecordTest do
  use ExUnit.Case, async: true

  doctest Record

  require Record

  test "extract/2 extracts information from an Erlang file" do
    assert Record.extract(:file_info, from_lib: "kernel/include/file.hrl") == [
             size: :undefined,
             type: :undefined,
             access: :undefined,
             atime: :undefined,
             mtime: :undefined,
             ctime: :undefined,
             mode: :undefined,
             links: :undefined,
             major_device: :undefined,
             minor_device: :undefined,
             inode: :undefined,
             uid: :undefined,
             gid: :undefined
           ]
  end
end
