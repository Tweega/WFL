defmodule Collocation.Producer do
  use GenStage

  def start_link(sent_map) do
    GenStage.from_enumerable(sent_map, name: __MODULE__)
  end

  def init(sent_map), do: {:producer, sent_map}

end
