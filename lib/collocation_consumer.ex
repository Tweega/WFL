defmodule Collocation.Consumer do
  use GenStage

  def start_link(work) do
    GenStage.start_link(__MODULE__, work)
  end

  def init(state) do
    case Process.whereis(Collocation.ProducerConsumer) do
      nil ->
        :ignore
      _ ->
        {:consumer, state}
    end
  end

  def handle_events(events, _from, {module , func, params} = state) do
    for event <- events do
      args = [event | params]
      apply(module, func, args)
    end

    # As a consumer we never emit events
    {:noreply, [], state}
  end

  def handle_info(_msg, state) do
    {:noreply, [], state}
  end
end
