
defmodule Collocation do
	require Logger

	def test({_key, wfl_item}) do
		wfl_item
	end

	def test2(wfl_item) do
		IO.inspect(wfl_item)
	end

	def add_collocs_to_wfl(collocs, wfl_pid) do
		#[{2, [{<<0, 0, 0, 6, 0, 0, 0, 5>>, 32}, {<<0, 0, 0, 7, 0, 0, 0, 6>>, 31},...]},
		#for each sentence
		inputs = Enum.reduce(collocs, [], fn({sent_id, token_offsets}, token_inputs_accum) ->
			#for each token_offset
			toke_inputs = Enum.reduce(token_offsets, [], fn({bin_token, offset}, token_inputs) ->
				#create instance
				instance = %TokenInstance{sentence_id: sent_id, offset: offset}
				[%TokenInput{token: bin_token, instance: instance}  | token_inputs]			
			end)
			toke_inputs ++ token_inputs_accum
		end)
		#add each input to the supplied wfl
		IO.inspect(inputs)
		Enum.each(inputs, fn(input) -> 			
			WFL.addToken(wfl_pid, input)	
		end)
		
		wfl_pid
	end


	def get_pairs({_key, wfl_item}, wfl_pid) do
		
		tokenID = wfl_item.type_id
IO.inspect("hello")
		inputs = Enum.reduce(wfl_item.instances, [], fn({sent_id, token_index}, new_sent_tokens) ->
			sent_bin_tokens = TokensBinary.get(sent_id).bin_tokens
			token_count = div(byte_size(sent_bin_tokens), 4)
			if token_index < token_count do
				ex_token = binary_part(sent_bin_tokens, (token_index) * 4, 4) #All we will ever want is a single extra token
				
				new_token =  tokenID <> ex_token
				#[{new_token, token_index} | new_sent_tokens]
				#token, sent_id, offset
				[%TokenInput{token: new_token, instance: %TokenInstance{sentence_id: sent_id, offset: token_index}} | new_sent_tokens]
			else
				new_sent_tokens
			end
		end)

		Enum.each(inputs, fn(input) -> 			
			{:ok, _token_id} = WFL.addToken(wfl_pid, input)	
		end)
		#IO.inspect(inputs)
		wfl_pid		
	end

	def extend_pairs(pair_pid) do
		#get list of pairs with freq > 2
		# get the list of wfl_items 		
		cutoff = 2
		sorted_wfl = WFLScratch.Server.get_sorted_wfl(pair_pid, :freq, :desc)
      	filtered_list = Enum.take_while(sorted_wfl, fn({_key, item}) -> item.freq >= cutoff end)

      

	end
	
	def get_sent_off_tokens(wfl_info_list) do		
		get_sent_off_tokens(wfl_info_list, [])
	end

	def get_sent_off_tokens([], acc) do 	#no wfl-infos left
		acc
	end

	def get_sent_off_tokens([wfl_info | tail], acc) do
		x = get_sent_off_tokes(wfl_info, acc)
		get_sent_off_tokens(tail, x)
	end

	def get_sent_off_tokes({_, %WFL_Type{} = wt}, acc) do	
		get_sent_off_tokes(wt.type_id, wt.instances, acc)
	end

	def get_sent_off_tokes(_type_id, [], acc) do 	#no more sentence instances
		acc
	end

	def get_sent_off_tokes(type_id, [{sent_id, offset} | tail], acc) do
		get_sent_off_tokes(type_id, tail, [{type_id, sent_id, offset} | acc])
	end

	def get_collocs({_key, wfl_item}) do
		#wfl_item:  %WFL_Type{freq: 2, instances: [{6, 9}, {2, 29}], type: "place", type_id: <<0, 0, 0, 9>>}
		
		#going to refactor so that first time round we only look at immediate pairs, then with triples to hollow out the phrase and to mark the hollow versions as possible abstractions
		#abstractions only remain in the wfl if their frequency is sufficiently greater than any one more specific phrasing.

		depth = 1	#this could derive from token_length
		tokenID = wfl_item.type_id
		offsets = [-3, -2, -1, 1, 2, 3]
		

		Enum.reduce(wfl_item.instances, [], fn({sent_id, token_index}, new_sent_tokens) ->
			#get tokens for the sentence	
			sent_bin_tokens = TokensBinary.get(sent_id).bin_tokens
			Logger.debug(sent_id)
			token_count = div(byte_size(sent_bin_tokens), 4)

			current_token_length = div(byte_size(tokenID), 4)

			new_tokes = Enum.reduce(offsets, [], fn(offset, new_tokens) ->
				zero_count = abs(offset) - 1
				new_token = <<>>
				new_tokes = nil

				if offset < 0 do
					index = token_index + offset

					if index > 0 do					
						ex_token = binary_part(sent_bin_tokens, (index - 1) * 4, 4) #All we will ever want is a single extra token
						
						if zero_count >  0 do					
							new_token = Enum.reduce(1..zero_count, <<>>, fn(_x, acc) -> <<0 :: integer-unit(8)-size(4)>> <> <<acc :: binary>> end)
						end
						new_token = ex_token <> new_token <> tokenID
						new_tokes = [{new_token, index} | new_tokens]
					end				
				else
					index = token_index + current_token_length + offset - 2
					
					if token_count > index do
						new_token = binary_part(sent_bin_tokens, index * 4, 4)
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

	def check_wfl(wfl_pid) do
		x = WFLScratch.Server.get_sorted_wfl(wfl_pid, :freq, :desc)
		IO.inspect(x)
	end
end