defmodule GenstageExample.Producer do
  use GenStage

  def start_link(wfl) do
    #GenStage.start_link(__MODULE__, wfl, name: __MODULE__)
    GenStage.from_enumerable(wfl, name: __MODULE__)
  end

  def init(wfl), do: {:producer, wfl}

  # def handle_demand(_demand, state) do
  #   {event, events} = case state do
  #     [] -> {[{:halt, nil}], []}
  #     [h | t] -> {[{:ok, h}], t}
  #   end

  #   {:noreply, event, events}
  # end

  def handle_info(msg, state) do
    IO.inspect({:p_info, msg})

    {:noreply, [], state}
  end
end
