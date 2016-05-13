defmodule WFLStream do

  #def get_wfl_stream(wfl_pid, _field, _filter,  cutoff) do
  def get_wfl_stream(wfl) do
    Stream.resource(
      fn -> 
      	wfl #list of wfl items.
      end,	#this fn intitialises the resource - it takes no params and returns 'the resource' - which will be a sorted wfl 
      fn(wfl_item_list) -> 
        case wfl_item_list do 	#return next wfl_item.  {:halt, accumulator} when finished.
        	[] -> {:halt, []}

			[wfl_item | rest] ->
            	{[wfl_item], rest}         
        end
      end,
      fn(empty_wfl) -> empty_wfl end 	#this takes accumulator, does clear up and returns the final value if there is one.
    )
  end


  def from(start) do
    Stream.resource(
      fn -> start end,
      fn(num) -> 
        case num do 
          num when num < 1000 -> 
            {[num + 1], num + 1} 
          _ ->
            {:halt, num}
        end
      end,
      fn(num) -> num end
    )
  end

end