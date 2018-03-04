defmodule PostgrexPreparedQuery do
  @name :prepared_query

  def start_link() do
    Agent.start_link(fn -> %{} end, name: @name)

  end

  def get_query_by_name(query_name) do
    Agent.get(:prepared_query, fn (query_map) ->
      Map.fetch!(query_map, query_name)
    end)
  end

  def set_query_by_name(query_name, query) do
    Agent.update(:prepared_query, fn(query_map) ->
      Map.put(query_map, query_name, query)
    end)
  end

end
