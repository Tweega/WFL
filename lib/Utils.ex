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



def rev_bin4(bin) do
    rev_bin4(bin, <<>>)
  end

  def rev_bin4(<<>>, acc) do
    acc
  end

  def rev_bin4(<<byte4 :: binary-size(4), rest :: binary>>, acc) do
    new_acc = << <<byte4 :: binary>>, <<acc :: binary>> >>
    rev_bin4(rest, new_acc)
  end



  def rev_bin(<< t :: binary >>) do
    rev_bin(t, <<>>)
  end

  def rev_bin(<<>>, store) do
    store
  end

  def rev_bin(<< h :: binary-size(1),  t :: binary >>, store) do
    #to convert number into binary :binary.encode_unsigned gives you smallest number of bytes for number
    #while <<256 :: size(16)>> gives you <<1, 0>>
    #and <<k :: integer-size(16) , rest :: binary>> = <<256 :: size(16)>> puts 256 into k 

    rev_bin(t, <<h <> store>>)

  end

  def dump_wfl(wfl_pid) do

    types = WFL.get_wfl(wfl_pid).types

    Enum.each(types, fn(t) ->
      IO.inspect(t)
    end)

  end
end 
