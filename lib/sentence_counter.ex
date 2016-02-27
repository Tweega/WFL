defmodule SentenceCounter do
  @name :sent_id_gen
  
  def start_link() do    
    Agent.start_link(fn -> 1 end, name: @name)
    
  end

  def get_sentence_id(pid) do
    Agent.get_and_update(pid, fn(n) -> {n, n + 1} end)
  end

  def set(pid, new_value) do
    #this should be for internal use only
    Agent.update(pid, fn(_n) -> new_value end)
  end

end