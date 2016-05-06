
defmodule TokensBinary do
  @name :tokens_bin
  
  def start_link() do        
    Agent.start_link(fn -> %{} end, name: @name)
  end

  def new(sentence_id, tokens_bin) do 
    Agent.update(:tokens_bin, &Map.put(&1, sentence_id, tokens_bin))
  end

  def get(sentence_id) do        
    Agent.get(:tokens_bin, &Map.get(&1, sentence_id))
  end

end