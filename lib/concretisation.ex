defmodule Concretisation do
  #Here we store concretisations for phrase_id, keeping reference to wf_pid also to know where to find token_info for the type.
  #map is populated when we expand all frequent phrases.  the expanded phrase is put here as key, the value a %Concretisation struct.
  # the concretisations field of the struct is populated in a second phase as we disassemble phrases with lose_one()

  defstruct([:wfl_pid, :phrase_id, concretisations: MapSet.new()])  #concretisations holds token_ids of types that extend the current type ie catsat extends cat and sat

  @name :concretisation
    
  def start_link() do        
    Agent.start_link(fn -> %{} end, name: @name)
  end

  def xnew(phrase_tokens, %Concretisation{} = concretisation_info) do     
    Agent.update(:concretisation, &Map.put(&1, phrase_tokens, concretisation_info))
  end

  def xget(phrase_tokens) do        
    Agent.get(:concretisation, &Map.get(&1, phrase_tokens))
  end

  def xadd_concretisation(abstraction_tokens, concretisation_id) do  
    Agent.update(:concretisation, &Map.update!(&1, abstraction_tokens, fn(%Concretisation{concretisations: concretisations} = conc) ->
        new_concretisations = MapSet.put(concretisations, concretisation_id)
        %Concretisation{conc | concretisations: new_concretisations }
    end))
  end

  def xget_map() do
    #we could make this (or sister function) into a stream
    Agent.get(:concretisation, &(&1)) #this returns agent state which is a map containing %{Concretisation} structs keyed on phrase expansions.
  end

# error message key <<0, 0, 0, 251, 0, 0, 0, 129>> not found in Map.update!/3 
    #in Concretisation.add_concretisation/2>}

end