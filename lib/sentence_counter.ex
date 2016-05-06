defmodule SentenceCounter do
  @name :sent_id_gen
  
  def start_link() do    
    Agent.start_link(fn -> 1 end, name: @name)
    
  end

  def get_sentence_id() do
    Agent.get_and_update(:sent_id_gen, fn(n) -> {n, n + 1} end)
  end

end