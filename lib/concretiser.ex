
defmodule Concretiser do
  @name :concretiser
  #this is only for debug purposes so as to be able to see which concretisations are associated with which abstractions
  #because of the way things are organised at the moment, it is not possible to et the transalation of
  #the token ids as this involves circular process calls.
  def start_link() do
    Agent.start_link(fn -> [] end, name: @name)
  end

  def new(abstraction, abstraction_pid, concretiser, concretiser_pid) do
    Agent.update(:concretiser, fn(list) ->
      [{abstraction, abstraction_pid, concretiser, concretiser_pid} | list]
    end)
  end



    def get(sentence_id) do
      Agent.get(:concretiser, &Map.get(&1, sentence_id))
    end

    def get_stream() do
      sent_map = Agent.get(:concretiser, &(&1)) #this returns agent state.
      sent_map |> Stream.map(&(&1))
    end

    def list_concretisations() do
      Agent.get(:concretiser, fn(list) ->
        Enum.each(list, fn({abs, abs_pid, concretiser_id, conc_pid} = jj) ->
            p = WFL.get_parent(abs_pid)
            abstraction = case p do
              nil ->
                abs
              _ ->
              abs_id = WFL.get_token_info(abs_pid, abs).type_id
              X_WFL.expand_type_id(abs_pid, abs_id)
            end
            concretiser = X_WFL.expand_type_id(conc_pid, concretiser_id)
            IO.inspect({abstraction, ":",  concretiser})
        end)
      end)
    end

end
