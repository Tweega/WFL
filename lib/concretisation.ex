
defmodule ProcessedPhrases do
  @name :processed_phrases
  #this will store phrases that have already been processed for concretisation.
  
  def start_link() do     
    #Agent.start_link(fn -> MapSet.new end, name: __MODULE__)   
    Agent.start_link(fn -> MapSet.new() end, name: @name)  #does this need to be widely available?
  end

  def new(phrase_id) do
    Agent.update(:processed_phrases, &MapSet.put(&1, phrase_id))
  end

  def contains(phrase_id) do        
    Agent.get(:processed_phrases, &MapSet.member?(&1, phrase_id))
  end
end