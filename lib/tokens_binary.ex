
defmodule TokensBinary do
  defstruct(bin_tokens: <<>>)

  @name :tokens_bin 
  
  def start_link() do        #have one of these per text file? one per wfl at any rate.
    Agent.start_link(fn -> %{} end, name: @name)
  end

  def new(sentence_id, %TokensBinary{} = tokens_bin) do 
    Agent.update(:tokens_bin, &Map.put(&1, sentence_id, tokens_bin))  #include sentence_id in with tokens_bin data in tuple?
  end

  def get(sentence_id) do        
    Agent.get(:tokens_bin, &Map.get(&1, sentence_id))
  end

  def get_map() do        
    Agent.get(:tokens_bin, &(&1))
  end
end