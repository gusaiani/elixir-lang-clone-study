Code.require_file("../test_helper.exs", __DIR__)

defmodule Mix.ReleaseTest do
  use MixTest.Case

  import Mix.Release
  doctest Mix.Release

  _ = Application.ensure_started(:eex)
  _ = Application.ensure_started(:runtime_tools)

  @erts_version :erlang.system_info(:version)
  @erts_source Path.join(:code.root_dir(), "erts-#{@erts_version}")
  @elixir_version Application.spec(:elixir, :vsn)
  @kernel_version Application.spec(:kernel, :vsn)
  @runtime_tools_version Application.spec(:runtime_tools, :vsn)
  @eex_ebin Application.app_dir(:eex, "ebin")

  setup do
    File.rm_rf!(tmp_path("mix_release"))
    File.mkdir_p!(tmp_path("mix_release"))
    :ok
  end

  describe "from_config!/3" do
    test "uses default configuration if no release is specified" do
      assert %Mix.Release{
               name: :mix,
               version: "0.1.0",
               path: path,
               version_path: version_path
             } = from_config!(nil, config(), [])

      assert String.ends_with?(path, "mix_release/_build/dev/rel/mix")
      assert String.ends_with?(version_path, "mix_release/_build/dev/rel/mix/releases/0.1.0")
    end

    test "provides default options" do
      release = from_config!(nil, config(), [])
      assert release.options == [overwrite: false, quiet: false, strip_beams: true]
    end

    test "allows overrides" do
      overrides = [path: "demo", version: "0.2.0", overwrite: true, quiet: true]
      release = from_config!(nil, config(), overrides)

      assert release.path == Path.absname("demo")
      assert release.version == "0.2.0"
      assert release.options[:overwrite]
      assert release.options[:quiet]
    end

    test "allows specifying the version from an application" do
      overrides = [version: {:from_app, :elixir}]
      release = from_config!(nil, config(), overrides)

      assert release.version == to_string(Application.spec(:elixir, :vsn))
    end

    test "raises when :from_app is used with an app that doesn't exist" do
      overrides = [version: {:from_app, :not_valid}]

      assert_raise Mix.Error,
                   ~r"Could not find version for :not_valid, please make sure the application exists",
                   fn -> from_config!(nil, config(), overrides) end
    end

    test "includes applications" do
      release = from_config!(nil, config(), [])
      assert release.applications.mix[:path] == to_charlist(Application.app_dir(:mix))
      refute release.applications.mix[:otp_app?]

      assert release.applications.kernel[:path] == to_charlist(Application.app_dir(:kernel))
      assert release.applications.kernel[:otp_app?]
    end

    test "uses chosen release via the CLI" do
      release =
        from_config!(
          :bar,
          config(releases: [foo: [version: "0.2.0"], bar: [version: "0.3.0"]]),
          []
        )

      assert release.name == :bar
      assert release.version == "0.3.0"
      assert String.ends_with?(release.path, "mix_release/_build/dev/rel/bar")

      assert String.ends_with?(
               release.version_path,
               "mix_release/_build/dev/rel/bar/releases/0.3.0"
             )
    end

    test "uses chosen release via the default_release" do
      release =
        from_config!(
          nil,
          config(
            default_release: :bar,
            releases: [foo: [version: "0.2.0"], bar: [version: "0.3.0"]]
          ),
          []
        )

      assert release.name == :bar
      assert release.version == "0.3.0"
      assert String.ends_with?(release.path, "mix_release/_build/dev/rel/bar")

      assert String.ends_with?(
               release.version_path,
               "mix_release/_build/dev/rel/bar/releases/0.3.0"
             )
    end

    test "raises for multiple releases and no name" do
      assert_raise Mix.Error,
                   ~r"\"mix release\" was invoked without a name but there are multiple releases",
                   fn -> from_config!(nil, config(releases: [foo: [], bar: []]), []) end
    end

    test "raises for unknown release" do
      assert_raise Mix.Error, "Unknown release :foo. The available releases are: []", fn ->
        from_config!(:foo, config(), [])
      end
    end

    test "uses the latest version of an app if there are multiple versions", context do
      in_tmp(context.test, fn ->
        test_erts_dir = Path.join(File.cwd!(), "erts-#{@erts_version}")
        test_libs_dir = Path.join(File.cwd!(), "lib")
        libs_dir = Path.join(:code.root_dir(), "lib")
        libs = File.ls!(libs_dir)

        File.cp_r!(@erts_source, test_erts_dir)

        for lib <- libs,
            source_file <- Path.wildcard(Path.join([libs_dir, lib, "ebin", "*.app"])) do
          target_dir = Path.join([test_libs_dir, lib, "ebin"])
          target_file = Path.join(target_dir, Path.basename(source_file))

          File.mkdir_p!(target_dir)
          File.cp!(source_file, target_file)
        end

        File.mkdir_p!(Path.join("lib", "compiler-1.0"))

        release = from_config!(nil, config(releases: [demo: [include_erts: test_erts_dir]]), [])

        assert Path.dirname(release.applications.compiler[:path]) == test_libs_dir
        assert release.applications.compiler[:vsn] != "1.0"
      end)
    end

    test "raises on unknown app" do
      assert_raise Mix.Error, "Could not find application :unknown", fn ->
        from_config!(nil, config(releases: [demo: [applications: [unknown: :none]]]), [])
      end
    end

    test "raises for missing version" do
      assert_raise Mix.Error, ~r"No :version found", fn ->
        from_config!(nil, config() |> Keyword.drop([:version]), [])
      end
    end

    test "raises on invalid release names" do
      assert_raise Mix.Error, ~r"Invalid release name", fn ->
        from_config!(nil, config(releases: ["invalid name": []]), [])
      end
    end
  end

  defp config(extra \\ []) do
    [
      app: :mix,
      version: "0.1.0",
      build_path: tmp_path("mix_release/_build"),
      config_path: tmp_path("mix_release/config/config.exs")
    ]
    |> Keyword.merge(extra)
  end
end
