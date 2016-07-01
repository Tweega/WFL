
defmodule Phrases do
  @name :phrases
  #the aim here will be to save off to a db eg mongodb or redis  Agent is temporary
  #phrases to be linked to a wfl?
  
  def start_link() do        
    Agent.start_link(fn -> %{} end, name: @name)
  end

  def new(phrase_id, phrase) do 
    #we need to swap tokens for type_ids
    
    Agent.update(:phrases, &Map.put(&1, phrase_id, phrase))
  end

  def get(phrase_id) do        
    Agent.get(:phrases, &Map.get(&1, phrase_id))
  end

  def get_map() do        
    Agent.get(:phrases, &(&1)) #this returns agent state.
  end
  
end