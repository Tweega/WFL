defmodule Collocation.ProducerConsumer do
  use GenStage

  require Integer

  def start_link do
    GenStage.start_link(__MODULE__, :state_doesnt_matter, name: __MODULE__)
  end

  def init(state) do
    {:producer_consumer, state, subscribe_to: [Collocation.Producer]}
  end

  def handle_events(events, _from, state) do

    {:noreply, events, state}
  end

end
