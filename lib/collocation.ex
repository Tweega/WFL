defmodule TokenFreq do
	defstruct([token_id: <<>>, freq: 0, index: -1])
end

defmodule Collocation do
	require Logger

	defstruct([token_ids: <<>>, concretisations: []])	#anything that further defines a pattern is a concretisation - though a shorter word would be nice.  ab is concretisation of a and b as is a_b.  acb is concretisation of a_b

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
			toke_inputs ++ token_inputs_accum	#better to recurse?
		end)
		#add each input to the supplied wfl
		#IO.inspect(inputs)
		Enum.each(inputs, fn(input) -> 			
			WFL.addToken(wfl_pid, input)	
		end)
		
		wfl_pid
	end


	def get_pairs_x({_key, wfl_item}, wfl_pid) do
		
		tokenID = wfl_item.type_id

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
						#IO.inspect(new_token)
						
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

	def say_hello({sentence_id, %TokensBinary{} = sent_bin_tokens}, colloc_wfl_pid) do
		#note that this function is being called in a parallel job one for each sentence
		source_wfl_pid = WFL.get_parent(colloc_wfl_pid)

		#get freqs for each token_id - TokenStream
		token_stream = TokenStream.get_token_stream(sent_bin_tokens.bin_tokens)	#using a stream to hand out token_ids into 4 byte chunks
		_token_count = div(byte_size(sent_bin_tokens.bin_tokens), 4)

		#get freqs for each token_id - map
		bin_tok_freq_list = Enum.map(token_stream, fn(tok_id) ->
			#get the wfl_info for this token
			wfl_info = WFL.get_token_info_from_id(source_wfl_pid, tok_id)
			%TokenFreq{token_id: tok_id, freq: wfl_info.freq}
		end)
		#IO.inspect(bin_tok_freq_list)
		#bin_tok_freq_list

		#now get pairs
		pairs = get_pairs(bin_tok_freq_list)
		IO.inspect(pairs)

		#next job is to merge pairs and add them to a new wfl - new wfl should be created at start of each pairing operation		

		#merged_pairs = merge_pairs(pairs, source_wfl_pid, colloc_wfl_pid)
	end

	def merge_pairs(pairs, source_wfl_pid, colloc_wfl_pid) do
		#go through each pair, and combine the two - also create gap abstractions where appropriate
		#try a reduce
		#Enum.reduce(pairs, acc, fun)
		nil
	end

	def merge_pairs([], accum) do
		accum
	end

	def merge_pairs([h | t], accum) do
		#for the moment just stick them together regardless of indices.
		#WORKING HERE
	end
	
	#i want to go through bin_tok_freq_list a,b   b, c  :  c,d etc until a pair is unobtainable
	
	def get_pairs(bin_tok_freq_list, cutoff \\ 2) do
		get_pairs({nil, nil}, bin_tok_freq_list, -1, cutoff, [])
	end

	def get_pairs(_pair, [], _index, _cutoff, accum) do
		accum
	end
	
	def get_pairs(pr, bin_tok_freq_list, index, cutoff, accum) do		
		{{a, b} = new_pair, new_toke_list, new_index} = get_pair(pr, bin_tok_freq_list, index, cutoff)		
		#IO.puts("a: #{a}\nb: #{b}\nindex: #{index}\nnew_index: #{new_index}")
		new_accum = 
			if not(is_nil(b) || is_nil(a) || new_index - index > cutoff) do				
				[new_pair | accum]
			else
				accum
			end
		get_pairs({b, nil}, new_toke_list, new_index, cutoff, new_accum)
	end

	def get_pair({%TokenFreq{}, %TokenFreq{}} = pair, token_freqs, index, _cutoff) do
		#we have a pair		
		{pair, token_freqs, index}
	end

	def get_pair({_, _}, [], index, _cutoff) do		
		#unable to complete a pair
		{{nil, nil}, [], index}	#right thing to return?
	end

	def get_pair({%TokenFreq{} = tf_a, _tf_b}, token_freqs, index, cutoff) do		
		#we have first of pair - get second.
		{tf_b, rest, new_index} = get_frequent_token(token_freqs, index, cutoff)
		new_tf_b = 
			if is_nil(tf_b) do
				nil 
			else
				%TokenFreq{tf_b | index: new_index }
			end
		get_pair({tf_a, new_tf_b}, rest, new_index, cutoff)
	end

	def get_pair({_, _}, token_freqs, index, cutoff) do		
		#we don't have any of the pair yet - get first
		{tf_a, new_list, new_index} = get_frequent_token(token_freqs, index, cutoff)

		new_tf_a = 
			if is_nil(tf_a) do
				nil 
			else
				%TokenFreq{tf_a | index: new_index }
			end

		#IO.inspect(tf_a)
		get_pair({new_tf_a, nil}, new_list, new_index, cutoff)
	end

	def get_frequent_token([], index, _cutoff) do		
		{nil, [], index}
	end

	def get_frequent_token([h | t], index, cutoff) do		
		if h.freq >= cutoff do			
			{h, t, index + 1}
		else
			get_frequent_token(t, index + 1, cutoff)	#we need to keep track of the token index also
		end
	end

end



defmodule TokenStream do
  def get_token_stream(bin_tokens) when is_binary(bin_tokens) do
    Stream.resource(
      fn ->
      	bin_tokens #list of wfl items.
      end,	#this fn intitialises the resource - it takes no params and returns 'the resource' - which will be a sorted wfl 
      fn(bin_tokes) -> 
        case bin_tokes do 	#return next wfl_item.  {:halt, accumulator} when finished.
        	<<>> -> {:halt, []}
        	<<token_id :: binary-size(4), token_ids :: binary>> ->
            	{[token_id], token_ids}
        end
      end,
      fn(empty_token_list) -> empty_token_list end 	#tidy up - returns the final value of the stream if there is one.
    )
  end

  def get_token_stream(bin_tokens) do
    IO.puts("not a binary then?")
    IO.inspect(bin_tokens)
  end
end
