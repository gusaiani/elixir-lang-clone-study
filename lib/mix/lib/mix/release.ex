defmodule Mix.Release do
  @moduledoc """
  Defines the release structure and convenience for assembling releases.
  """

  @doc """
  The Mix.Release struct has the following read-only fields:

    * `:name` - the name of the release as an atom
    * `:version` - the version of the release as a string or
       `{:from_app, app_name}`
    * `:path` - the path to the release root
    * `:version_path` - the path to the release version inside the release
    * `:applications` - a map of application with their definitions
    * `:erts_source` - the erts source as a charlist (or nil)
    * `:erts_version` - the erts version as a charlist

  The following fields may be modified as long as they keep their defined types:

    * `:boot_scripts` - a map of boot scripts with the boot script name
      as key and a keyword list with **all** applications that are part of
      it and their modes as value
    * `:config_providers` - a list of `{config_provider, term}` tuples where the
      first element is a module that implements the `Config.Provider` behaviour
      and `term` is the value given to it on `c:Config.Provider.init/1`
    * `:options` - a keyword list with all other user supplied release options
    * `:steps` - a list of functions that receive the release and returns a release.
      Must also contain the atom `:assemble` which is the internal assembling step.
      May also contain the atom `:tar` to create a tarball of the release.

  """
  defstruct [
    :name,
    :version,
    :path,
    :version_path,
    :applications,
    :boot_scripts,
    :erts_source,
    :erts_version,
    :config_providers,
    :options,
    :steps
  ]

  @type mode :: :permanent | :transient | :temporary | :load | :none
  @type application :: atom()
  @type t :: %{
          name: atom(),
          version: String.t(),
          path: String.t(),
          version_path: String.t() | {:from_app, application()},
          applications: %{application() => keyword()},
          boot_scripts: %{atom() => [{application(), mode()}]},
          erts_version: charlist(),
          erts_source: charlist() | nil,
          config_providers: [{module, term}],
          options: keyword(),
          steps: [(t -> t) | :assemble, ...]
        }

  @default_apps [kernel: :permanent, stdlib: :permanent, elixir: :permanent, sasl: :permanent]
  @safe_modes [:permanent, :temporary, :transient]
  @unsafe_modes [:load, :none]
  @significant_chunks ~w(Atom AtU8 Attr Code StrT ImpT ExpT FunT LitT Line)c
  @copy_app_dirs ["priv"]

  @doc false
  @spec from_config!(atom, keyword, keyword) :: t
  def from_config!(name, config, overrides) do
    {name, apps, opts} = find_release(name, config)

    unless Atom.
  end

  defp find_release(name, config) do
    {name, opts} = lookup_release(name, config) || infer_release(config)
  end

  defp lookup_release(nil, config) do
    case Keyword.get(config, :releases, []) do
      [] ->
        nil

      [{name, opts}] ->
        {name, opts}

      [_ | _] ->
        case Keyword.get(config, :default_release) do
          nil ->
            Mix.raise(
              "\"mix release\" was invoked without a name but there are multiple releases. " <>
                "Please call \"mix release NAME\" or set :default_release in your project configuration"
            )

          name ->
            lookup_release(name, config)
        end
    end
  end

  defp lookup_release(name, config) do
    if opts = config[:releases][name] do
      {name, opts}
    else
      found = Keyword.get(config, :releases, [])

      Mix.raise(
        "Unknown release #{inspect(name)}." <>
          "The available releases are: #{inspect(Keyword.keys(found))}"
      )
    end
  end

  defp infer_release(config) do
    if Mix.Project.umbrella?(config) do
      bad_umbrella!()
    else
      {Keyword.fetch!(config, :app), []}
    end
  end
end
