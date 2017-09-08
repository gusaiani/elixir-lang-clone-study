defmodule Mix.Local do
  @moduledoc false

  @public_keys_html "https://repo.hex.pm/installs/public_keys.html"

  @type item :: :archive | :escript

  @doc """
  Returns the name for an archive or an escript, based on the project config.

  ## Examples

      iex> Mix.Local.name_for(:archive, [app: "foo", version: "0.1.0"])
      "foo-0.1.0.ez"

      iex> Mix.Local.name_for(:escript, [escript: [name: "foo"]])
      "foo"

  """
  @spec name_for(item, keyword) :: String.t
  def name_for(:archive, project) do
    version = if version = project[:version], do: "-#{version}"
    "#{project[:app]}#{version}.ez"
  end

  def name_for(:escript, project) do
    case get_in(project, [:escript, :name]) do
      nil -> project[:app]
      name -> name
    end |> to_string()
  end

  @doc """
  The path for local archives or escripts.
  """
  @spec path_for(item) :: String.t
  def path_for(:archive) do
    System.get_env("MIX_ARCHIVES") || Path.join(Mix.Utils.mix_home, "archives")
  end
