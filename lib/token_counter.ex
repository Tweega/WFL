defmodule TokenCounter do
  def new do
    Agent.start_link(fn -> 1 end)
  end

  def get_token_id(pid) do
    Agent.get_and_update(pid, fn(n) -> {<<n :: integer-unit(8)-size(4)>>, n + 1} end)
  end

  def set(pid, new_value) do
    #this should be for internal use only
    Agent.update(pid, fn(_n) -> new_value end)
  end

end