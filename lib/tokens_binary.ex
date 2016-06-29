
defmodule TokensBinary do
  #stores a binary representation of a sentence <<11, 43, 41, 2, 83>> - keyed on sentence, where each number represents a wrod such as cat.  numbers will actually be 4 bytes
  #also stores a map between first offset and token
  defstruct(bin_tokens: <<>>, offset_map: %{})

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

  def get_bin_tokens(sentence_id) do        
    Agent.get(:tokens_bin, fn(state) ->
      x = Map.get(state, sentence_id)
      x.bin_tokens
    end)
  end

  def get_offset_map(sentence_id) do        
    Agent.get(:tokens_bin, fn(state) ->
      x = Map.get(state, sentence_id)
      x.bin_tokens
    end)
  end

  def get_map() do        
    Agent.get(:tokens_bin, &(&1)) #this returns agent state.
  end
end