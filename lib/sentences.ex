
defmodule Sentences do
  @name :sentences
  #the aim here will be to save off to a db eg mongodb or redis  Agent is temporary
  
  def start_link() do        
    Agent.start_link(fn -> %{} end, name: @name)
  end

  def new(sentence_id, sentence) do 
    #we need to swap tokens for type_ids
    
    Agent.update(:sentences, &Map.put(&1, sentence_id, sentence))
  end

  def get(sentence_id) do        
    Agent.get(:sentences, &Map.get(&1, sentence_id))
  end

end