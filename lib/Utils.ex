defmodule Utils do
  def compare(data_item, search_item) do
    cond do
      data_item > search_item -> -1
      search_item > data_item -> 1
      true -> 0
    end
  end

  def compare_ranges(range, _element) when is_nil(range) do
    -1  #having sid which, range should never be nil
  end

  def compare_ranges(first_range..last_range, first_search..last_search)  do
    
    cond do
      first_range > first_search -> -1
      last_search > last_range -> 1
      true -> 0
    end
  end

  def compare_ranges(first..last, search_item)  do
    
    cond do
      first > search_item -> -1
      search_item > last -> 1
      true -> 0
    end
  end


  def compare_char_type(data, _element) when is_nil(data) do
    -1  #having said which, data should never be nil
  end

  def compare_char_type(%{:range => first_data..last_data}, %{:range => first_search..last_search})  do
    
    cond do
      first_data > first_search -> -1
      last_search > last_data -> 1
      true -> 0
    end
  end

  def compare_char_type(%{:range => first..last}, search_item)  do
    
    cond do
      first > search_item -> -1
      search_item > last -> 1
      true -> 0
    end
  end


  def test(str) do
    # << j :: utf8 >> = x; 
    #for <<c <- " hello world ">>, c != ?\s, into: "", do: c  
    #x = for c <- String.codepoints("Helloé") do <<ch :: utf8>> = c; ch end  
    Stream.resource(fn -> String.codepoints(str) end,
      fn str ->
        case str do
          [h|t] -> {[h], t}
          _ -> {:halt, []}
        end
      end,
      fn _ -> nil end)
    end


end 
