defmodule Scratch do
require Logger
#to do - review use of <> which is equiv to ++ .  Use <<a, b>> form where possible
use Bitwise

def new()  do
	tokens = Map.new()
	cat = to_4_bytes([87])	#tokens will have a binary id made up of 4 byte integers representing tokens
	
	tokens = Map.put(tokens, "cat", cat)	#lookup for token ids form token
	#we will need anoter dictionary keyed on tokenID

	token_events = Map.new()    
	#structure is {sentenceID, tokenOffset}
	

	token_events = Map.put(token_events, cat, [{"s1", 3}, {"s2", 2}, {"s3", 1}, {"s4", 0}, {"s5", 6}])	#cat = <<0, 0, 0, 87>>

	sent_tokens = Map.new()
	
	sent_tokens = Map.put(sent_tokens, "s1", to_4_bytes([97, 21, 67, 87, 22, 33, 44]))	
	sent_tokens = Map.put(sent_tokens, "s2", to_4_bytes([197, 121, 87, 871, 122, 331, 144]))
	sent_tokens = Map.put(sent_tokens, "s3", to_4_bytes([917, 87, 617, 817, 212, 313, 414]))	
	sent_tokens = Map.put(sent_tokens, "s4", to_4_bytes([87, 211, 671, 187, 221, 133, 441]))	
	sent_tokens = Map.put(sent_tokens, "s5", to_4_bytes([2971, 2211, 12321, 2187, 2221, 1233, 87]))
	
	{tokens, token_events, sent_tokens}
end

def to_4_bytes(l) do
	Enum.reduce(Enum.reverse(l), <<>>, fn(t, acc) -> << <<t :: integer-unit(8)-size(4)>> <> acc >> end)
end


def make1(token) do
	#from {"s1", 2} produce [{"s1", [{<<0, 0, 4, 209, 0, 0, 0, 87>>, 2},...]}]

	{tokens, token_events, sent_tokens} = new()

	tokenID = Map.get(tokens, token)
	

	offsets = [-3, -2, -1, 1, 2, 3]
	 
	token_offs = Map.get(token_events, tokenID)

	Enum.reduce(token_offs, [], fn({sent_id, token_index}, new_sent_tokens) ->
		temp_tokes = Map.get(sent_tokens, sent_id)   #to_4_bytes([97, 21, 67, 87, 22, 33, 44])
		Logger.debug(sent_id)
		token_count = div(byte_size(temp_tokes), 4)

		current_token_length = div(byte_size(tokenID), 4)

		new_tokes = Enum.reduce(offsets, [], fn(offset, new_tokens) ->
			zero_count = abs(offset) - 1
			new_token = <<>>
			new_tokes = nil

			if offset < 0 do
				index = token_index + offset

				if index > -1 do					
					ex_token = binary_part(temp_tokes, index * 4, 4) #All we will ever want is a single extra token
					
					if zero_count >  0 do					
						new_token = Enum.reduce(1..zero_count, <<>>, fn(_x, acc) -> <<0 :: integer-unit(8)-size(4)>> <> <<acc :: binary>> end)
					end
					new_token = ex_token <> new_token <> tokenID
					new_tokes = [{new_token, index} | new_tokens]
				end				
			else
				index = token_index + current_token_length + offset - 1
				Logger.debug(index)

				if token_count > index do
					new_token = binary_part(temp_tokes, index * 4, 4)
					IO.inspect(new_token)
					
					if zero_count >  0 do
						new_token = Enum.reduce(1..zero_count, new_token, fn(_x, acc) -> <<0 :: integer-unit(8)-size(4)>> <> <<acc :: binary>> end)
					end

					new_token = tokenID <> new_token
					new_tokes = [{new_token, token_index} | new_tokens]
				end
			end
			new_tokes || new_tokens
		end)
		[{sent_id, new_tokes} | new_sent_tokens]
	end)
end


def mask_tokens(<<num :: binary>>, mask) do 
	num_bytes = byte_size(num)
	<<x :: integer-unit(8)-size(num_bytes)>> = num
	mask_bin = mask_to_bits(mask)
	nb = byte_size(mask_bin)
	<<y :: integer-unit(8)-size(nb)>> = mask_bin
	masked_bin = x &&& y
	:binary.encode_unsigned(masked_bin)
end

def mask_to_bits(mask) do
	Enum.reduce(Enum.reverse(mask), <<>>, fn(bit, acc) ->
		if bit == 0 do
			<<0x00 :: integer-unit(8)-size(4)>> <> <<acc :: binary>>
		else
			:erlang.list_to_binary(List.duplicate(0xFF, 4)) <> <<acc :: binary>>
		end
	end)
end



def maskbits(bits, mask) do
	rev_bin(maskbits(bits, mask, <<>>))
end

def maskbits(_bits, [], acc) do
	acc
end


def maskbits(<<int :: binary-size(1), bits_rest :: binary>>, [mask | mask_rest], acc) do
	
	acc2 = 
		if mask == 0 do
			<< <<0>> <> acc >>
		else
			<< <<int :: binary-size(1) >> <> acc >>
		end

	maskbits(bits_rest, mask_rest, acc2)

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


end