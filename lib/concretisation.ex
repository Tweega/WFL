
defmodule Concretisation do
  
  defstruct([:phrase_id, concretisations: []])  #concretisations holds token_ids of types that extend the current type ie catsat extends cat and sat

  @name :concretisation
  #this will store phrases that have already been processed for concretisation.
    
  def start_link() do        
    Agent.start_link(fn -> %{} end, name: @name)
  end

  def new(phrase_tokens, %Concretisation{} = concretisation_info) do     
    Agent.update(:concretisation, &Map.put(&1, phrase_tokens, concretisation_info))
  end

  def get(phrase_tokens) do        
    Agent.get(:concretisation, &Map.get(&1, phrase_tokens))
  end

  def add_concretisation(abstaction_tokens, concretisation_id) do        
    Agent.update(:concretisation, &Map.update(&1, fn(map) ->
      Map.update!(map, abstaction_tokens, fn(%Concretisation{phrase_id: phrase_id, concretisations: concretisations}) ->
        %Concretisation{phrase_id: phrase_id, concretisations: [concretisation_id | concretisations] }
      end)
    end))
  end

end