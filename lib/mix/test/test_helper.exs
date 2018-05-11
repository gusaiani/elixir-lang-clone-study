Mix.start()
Mix.shell(Mix.Shell.Process)
Application.put_env(:mix, :colors, enabled: false)

exclude = if match?({:win32, _}, :os.type()), do: [unix: true], else: [windows: true]
ExUnit.start(trace: "--trace" in System.argv(), exclude: exclude)

unless {1, 7, 4} <= Mix.SCM.Git.git_version() do
  IO.puts(:stderr, "Skipping tests with git sparse checkouts...")
  ExUnit.configure(exclude: :git_sparse)
end

# Clear proxy variables that may affect tests
System.delete_env("http_proxy")
System.delete_env("https_proxy")
System.delete_env("HTTP_PROXY")
System.delete_env("HTTPS_PROXY")

defmodule MixTest.Case do
  use ExUnit.CaseTemplate

  defmodule Sample do
    def project do
      [app: :sample, version: "0.1.0", aliases: [sample: "compile"]]
    end
  end

  using do
    quote do
      import MixTest.Case
    end
  end

  setup config do
    if apps = config[:apps] do
      Logger.remove_backend(:console)
    end

    on_exit(fn ->
      Application.start(:logger)
      Mix.env(:dev)
      Mix.Task.clear()
      Mix.Shell.Process.flush()
      Mix.ProjectStack.clear_cache()
      Mix.ProjectStack.clear_stack()
      delete_tmp_paths()

      if apps do
        for app <- apps do
          Application.stop(app)
          Application.unload(app)
        end

        Logger.add_backend(:console, flush: true)
      end
    end)

    :ok
  end

  def fixture_path do
    Path.expand("fixtures", __DIR__)
  end

  def fixture_path(extension) do
    Path.join(fixture_path(), extension |> to_string() |> String.replace(":", ""))
  end

  def tmp_path do
    Path.expand("../tmp", __DIR__)
  end

  def tmp_path(extension) do
    Path.join(tmp_path(), extension |> to_string() |> String.replace(":", ""))
  end
