defmodule TokenID do
  @name :token_id_gen #if there is to be more than one text processor then these will have to be local to the processor.
  
  def start_link() do
    Agent.start_link(fn -> {1, %{}} end, name: @name)
    
  end

  def get_token_id(pid) do   #should callers have to know pid?
    #this is called when we have a new token and need to determine its type id
    #we need to see if this type already exists, otherwise create entry for it.  return token-id as 4 bytes.

    Agent.get_and_update(pid, fn({next_id, map}) -> {{next_id, n + 1} end)
  end

  def set(pid, new_value) do
    #this should be for internal use only
    Agent.update(pid, fn(_n) -> new_value end)
  end

end