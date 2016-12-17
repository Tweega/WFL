defmodule ExpansionItem do
  defstruct([:wfl_pid, :phrase_id])
end

defmodule Expansion do
  #Here we store expansions for phrase_id, keeping reference to wf_pid also to know where to find token_info for the type.
  #map is populated when we expand all frequent phrases.  the expanded phrase is put here as key, the value a %Expansion struct.
  # the expansions field of the struct is populated in a second phase as we disassemble phrases with lose_one()
  
  defstruct([:root_wfl_pid, :root_colloc_pid, :expansion_map, :phrase_map]) #phrase is always 2 tokens, expansion has a token per word in the phrase
  
  @name :expansion
    
  def start_link(root_wfl_pid, root_colloc_pid) do   
    Agent.start_link(fn -> %Expansion{root_wfl_pid: root_wfl_pid, root_colloc_pid: root_colloc_pid, expansion_map: %{}, phrase_map: %{}} end, name: @name)
  end

  def new(phrase_tokens, %ExpansionItem{phrase_id: phrase_id} = expansion_item) do     
    Agent.update(:expansion, fn (%Expansion{root_wfl_pid: root_wfl_pid, root_colloc_pid: root_colloc_pid, expansion_map: expansion_map, phrase_map: phrase_map} = expansion) -> 
        new_expansion_map = Map.put(expansion_map, phrase_tokens, expansion_item)
        new_phrase_map = Map.put(phrase_map, phrase_id, phrase_tokens)
        %Expansion{expansion | expansion_map: new_expansion_map, phrase_map: new_phrase_map}
    end)
  end

  def get_phrase_id(phrase_tokens) do        
    Agent.get(:expansion, fn (%Expansion{expansion_map: expansion_map}) -> 
      Map.get(expansion_map, phrase_tokens)
    end)
  end

  def get_phrase(phrase_id) do        
    Agent.get(:expansion, fn (%Expansion{phrase_map: phrase_map}) -> 
      Map.get(phrase_map, phrase_id)
    end)
  end

  def add_concretisation(abstraction_tokens, concretisation_id) do

    %Expansion{root_wfl_pid: root_wfl_pid, root_colloc_pid: root_colloc_pid, expansion_map: expansion_map, phrase_map: phrase_map} = 
      Agent.get(:expansion, fn (expansion) -> 
        expansion
      end)

    case abstraction_tokens do
      <<_ :: binary-size(4)>> ->
        #single token - update concretisations in root wfl
        WFL.add_concretisation(root_wfl_pid, abstraction_tokens, concretisation_id, false)

      <<_ :: binary-size(8)>> ->
        #2 tokens - update concretisations in root collocation wfl        
        WFL.add_concretisation(root_colloc_pid, abstraction_tokens, concretisation_id, false)

      _ ->
        # more than 2 tokens - look up the expansion to find which wfl to update
        #IO.inspect({:abstraction_tokens, abstraction_tokens})
        #%ExpansionItem{wfl_pid: wfl_pid, phrase_id: phrase_id} = Map.get(expansion_map, abstraction_tokens)
        case Map.get(expansion_map, abstraction_tokens) do
          nil ->
            IO.inspect({:not_found, abstraction_tokens})
          :not_found

        %ExpansionItem{wfl_pid: wfl_pid, phrase_id: phrase_id} ->
          WFL.add_concretisation(wfl_pid, phrase_id, concretisation_id, true)

        _ ->
          :hmmm
        end
    end    
  end

  def get_expansion_map() do
    #we could make this (or sister function) into a stream
    Agent.get(:expansion, fn (%Expansion{expansion_map: expansion_map}) -> 
      expansion_map
    end)
  end

  #get_phrase_map?


# error message key <<0, 0, 0, 251, 0, 0, 0, 129>> not found in Map.update!/3 
    #in Expansion.add_expansion/2>}

end
