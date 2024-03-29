# defmodule OffsetMaps do
#   defstruct([token_map: %{}, combination_map: %{}])
# end

defmodule TokensBinary do
  #stores a binary representation of a sentence <<11, 43, 41, 2, 83>> - keyed on sentence,
  #where each number represents a word such as cat.  numbers will actually be 4 bytes
  #also stores a map between first offset and token
  defstruct(bin_tokens: <<>>, offset_maps: %OffsetMaps{})

  @name :tokens_bin

  def start_link() do        #have one of these per text file? one per wfl at any rate.
    Agent.start_link(fn -> %{} end, name: @name)
  end

  def new(sentence_id, %TokensBinary{} = tokens_bin) do
    Agent.update(:tokens_bin, &Map.put(&1, sentence_id, tokens_bin))  #include sentence_id in with tokens_bin data in tuple?
  end

  def update(sentence_id, %TokensBinary{} = tokens_bin) do
    #IO.puts("updating for: #{sentence_id}")
    Agent.update(:tokens_bin, &Map.update!(&1, sentence_id, fn(_x) -> tokens_bin end))
  end

  def set_offset_maps(sentence_id, toke_bin, token_map, combination_map) do
    new_tokens_binary = %TokensBinary{toke_bin | offset_maps: {token_map, combination_map}}
    Agent.update(:tokens_bin, &Map.update!(&1, sentence_id, new_tokens_binary))
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

  def get_offset_maps(sentence_id) do
    Agent.get(:tokens_bin, fn(state) ->
      x = Map.get(state, sentence_id)
      x.offset_maps
    end)
  end

  def get_map() do
    Agent.get(:tokens_bin, &(&1)) #this returns agent state.
  end

  def get_stream() do
    sent_map = Agent.get(:tokens_bin, &(&1)) #this returns agent state.
    sent_map |> Stream.map(&(&1))
  end
end
