defmodule GenstageExample.ProducerConsumer do
  use GenStage

  require Integer

  def start_link do
    GenStage.start_link(__MODULE__, :state_doesnt_matter, name: __MODULE__)
  end

  def init(state) do
    {:producer_consumer, state, subscribe_to: [GenstageExample.Producer]}
  end

  def handle_events(events, _from, state) do
    IO.inspect({:pc, events})

    {:noreply, events, state}
  end

  def handle_info(msg, state) do
    IO.inspect({:pc_info, msg})

    {:noreply, [], state}
  end
end
