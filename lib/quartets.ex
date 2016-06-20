
defmodule Quartets do
  @name :quartets
  #the aim here will be to save off to a db eg mongodb or redis  Agent is temporary
  #quartets to be linked to a wfl?
  
  def start_link() do        
    Agent.start_link(fn -> %{} end, name: @name)
  end

  def new(quartet_id, quartet) do 
    #we need to swap tokens for type_ids
    
    Agent.update(:quartets, &Map.put(&1, quartet_id, quartet))
  end

  def get(quartet_id) do        
    Agent.get(:quartets, &Map.get(&1, quartet_id))
  end

end