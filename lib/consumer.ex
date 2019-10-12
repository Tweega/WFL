defmodule GenstageExample.Consumer do
  use GenStage

  def start_link do
    GenStage.start_link(__MODULE__, :state_doesnt_matter)
  end

  def init(state) do
    {:consumer, state}
  end

  def handle_events(events, _from, state) do
    IO.inspect("hello")
    IO.inspect({:events, events})
    for event <- events do
      IO.inspect({self(), event, state})
    end
    IO.inspect("goodbye")

    # As a consumer we never emit events
    {:noreply, [], state}
  end

  def handle_info(msg, state) do
    IO.inspect({:c_info, msg})

    {:noreply, [], state}
  end
end
