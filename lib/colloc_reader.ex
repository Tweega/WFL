defmodule CollocReader do
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
			toke_inputs = Enum.reduce(token_offsets, token_inputs_accum, fn({bin_token, offset}, token_inputs) ->
				#create instance
				instance = %TokenInstance{sentence_id: sent_id, offset: offset}
				[%TokenInput{token: bin_token, instance: instance}  | token_inputs]			
			end)
			[toke_inputs | token_inputs_accum]
		end)
		#add each input to the supplied wfl
		Enum.each(inputs, fn(input)->
			WFL.addToken(wfl_pid, input)
		end)
		wfl_pid
	end


	def get_collocs({_key, wfl_item}) do
		#wfl_item:  %WFL_Type{freq: 2, instances: [{6, 9}, {2, 29}], type: "place", type_id: <<0, 0, 0, 9>>}
		#wfl_item:  %WFL_Type{freq: 2, instances: [{6, 9}, {2, 29}], type: "place", type_id: <<0, 0, 0, 9>>}
		#Logger.debug("do we get here?")
		#for each instance get binary tokens for the sentence, then do the business with pairing them up
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
end
