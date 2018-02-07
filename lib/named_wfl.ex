
defmodule NamedWFL do
  @name :named_wfl
  #stores two key pids against names, root_wfl_pid and last_wfl_pid
  #we might want to have root_colloc_pid also.

  def start_link() do
    Agent.start_link(fn -> Map.new() end, name: @name)
  end

  def new(wfl_name, pid) do
    Agent.update(:named_wfl, &Map.put(&1, wfl_name, pid))
  end

  def get_pid_from_name(wfl_name) do
    Agent.get(:named_wfl, &Map.get(&1, wfl_name))
  end

  def get_map() do
    Agent.get(:named_wfl, &(&1))
  end
end
