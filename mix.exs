defmodule WflScratch.Mixfile do
  use Mix.Project

  def project do
    [app: :wfl_scratch,
     version: "0.0.1",
     elixir: "~> 1.1",
     build_embedded: Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     deps: deps()]
  end

  # Configuration for the OTP application
  #
  # Type "mix help compile.app" for more information
  def application do
    [registered: [:WFL, :sent_id_gen, :token_id_gen], #this list is incomplete tk
    extra_applications: [:logger],
    mod: {WFLScratch, [:hello]}] #application will call WFLScratch.start/1.
  end

  # Dependencies can be Hex packages:
  #
  #   {:mydep, "~> 0.3.0"}
  #
  # Or git/path repositories:
  #
  #   {:mydep, git: "https://github.com/elixir-lang/mydep.git", tag: "0.1.0"}
  #
  # Type "mix help deps" for more examples and options
  defp deps do
      [{:earmark, "~> 0.1", only: :dev},
      {:ex_doc, "~> 0.11", only: :dev},
      {:postgrex, "~> 0.13.5"},
      {:poolboy, "~> 1.5" },
      {:poison, "~> 3.1"}]
  end
end
