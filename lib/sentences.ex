defmodule Sentences do
  @name :sentences
  
  def start_link() do        
    Agent.start_link(fn -> %{} end, name: @name)  #should be a struct  - this returns agent's pid  
  end

  def new(sentence_id, token_array) do 
    #we need to swap tokens for type_ids
    Enum.map(token_array, fn toke ->
      toke_id = TokenCounter.get_token_id()
    end)
    Agent.update(@name, &Map.put(&1, sentence_id, token_array))
  end

  def get(sentence_id) do        
    Agent.get(@name, &Map.get(&1, sentence_id))
  end

end