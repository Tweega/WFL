defmodule TokenCounter do
  @name :token_id_gen
  
  def start_link() do    
    Agent.start_link(fn -> 1 end, name: @name)  
  end

  def get_token_id() do
    Agent.get_and_update(:token_id_gen, fn(n) -> {<<n :: integer-unit(8)-size(4)>>, n + 1} end)
  end

  def set(new_value) do
    #this should be for internal use only
    Agent.update(:token_id_gen, fn(_n) -> new_value end)
  end

end


