
defmodule Concretisation do
  
  defstruct([:wfl_pid, :phrase_id, concretisations: MapSet.new()])  #concretisations holds token_ids of types that extend the current type ie catsat extends cat and sat

  @name :concretisation
    
  def start_link() do        
    Agent.start_link(fn -> %{} end, name: @name)
  end

  def new(phrase_tokens, %Concretisation{} = concretisation_info) do     
    Agent.update(:concretisation, &Map.put(&1, phrase_tokens, concretisation_info))
  end

  def get(phrase_tokens) do        
    Agent.get(:concretisation, &Map.get(&1, phrase_tokens))
  end

  def add_concretisation(abstraction_tokens, concretisation_id) do  
    Agent.update(:concretisation, &Map.update!(&1, abstraction_tokens, fn(%Concretisation{concretisations: concretisations} = conc) ->
        new_concretisations = MapSet.put(concretisations, concretisation_id)
        %Concretisation{conc | concretisations: new_concretisations }
    end))
  end

  def get_map() do
    #we could make this (or sister function) into a stream
    Agent.get(:concretisation, &(&1)) #this returns agent state which is a map containing %{Concretisation} structs keyed on phrase expansions.
  end

end