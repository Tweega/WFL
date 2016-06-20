defmodule QuartetCounter do  	#have a generic module to create these storage buckets?
  @name :quart_id_gen
  
  def start_link() do    
    Agent.start_link(fn -> 1 end, name: @name)
    
  end

  def get_quartet_id() do
    Agent.get_and_update(:sent_id_gen, fn(n) -> {n, n + 1} end)
  end

end