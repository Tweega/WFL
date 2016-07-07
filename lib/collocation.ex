defmodule TokenInput do
		defstruct([:token, :instance])
	end


defmodule TokenInstance do
		defstruct([:sentence_id, :offset])
	end

defmodule TokenFreq do
	defstruct([token_id: <<>>, freq: 0, index: -1, offset: -1, is_common: false])
end

defmodule TokenAbstractions do
	defstruct([token: <<>>, abstractions: [] ])
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
		cutoff = get_cutoff()
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

	
	def check_wfl(wfl_pid) do
		x = WFLScratch.Server.get_sorted_wfl(wfl_pid, :freq, :desc)
		IO.inspect(x)
	end

	def say_hello({sentence_id, %TokensBinary{bin_tokens: bin_tokens} = tb}, colloc_wfl_pid) do
		#note that this function is being called in a parallel job one for each sentence
		source_wfl_pid = WFL.get_parent(colloc_wfl_pid)
		cutoff = get_cutoff()

		#get freqs for each token_id - TokenStream
		token_stream = TokenStream.get_token_stream(bin_tokens)	#using a stream to hand out token_ids into 4 byte chunks
		
		#get freqs for each token_id - map
		{bin_tok_freq_list, index_map, _ndx} = Enum.reduce(token_stream, {[], %{}, 0}, fn(tok_id, {list_acc, map_acc, index}) ->
			#get the wfl_info for this token
			wfl_info = WFL.get_token_info_from_id(source_wfl_pid, tok_id)
			tok_freq = %TokenFreq{token_id: tok_id, freq: wfl_info.freq, is_common: wfl_info.is_common, offset: index}
			#IO.inspect(tok_freq)
			new_list_acc = [tok_freq | list_acc]
			new_map = Map.put(map_acc, index, tok_freq)
			{new_list_acc, new_map, index + 1}
		end)

		#index_map maps token offsets with token ids
		_sample_index =  %{12 => %TokenFreq{freq: 1, index: -1, is_common: false, offset: 12, token_id: <<0, 0, 0, 52>>}}

		#IO.inspect(index_map)

		#IO.inspect(bin_tok_freq_list)
		
		#split this list on sequences of tokens with freq > cutoff or where the gap is less than 3 ignore phrases of length 1 only
		#changing phrases to record only offsets not tokenfreq records
		#to revert go to commit a52d471101afcc79f63e5b614ea78cc6d03bf10d
		phrases = get_phrases(bin_tok_freq_list, cutoff)
		#IO.inspect(phrases)
		#bin_tok_freq_list
		_sample_phrases = [[12, 11, 9, 7, 5, 4, 3, 1, 0], [32, 31, 30, 28, 27, 25]] 	#strings of proximate tokens with freq > c/o

		#--chained(fData, fCx) fData is func for quartets which will initially return nil.  
		# fCx is called on data is nil and returns a new data item and data continuation func and cx cont func

		nilFunc = PhraseStream.unchained(nil)	#this is initial setting for quartets - data
		fPhrase = PhraseStream.unchained(phrases)
		fQuartetExtractor = PhraseStream.quartet_extractor(fPhrase)

		fQuartets = PhraseStream.chained(nilFunc, fQuartetExtractor)

		#quartets = PhraseStream.chained(phrase_stream)
		quartet_stream = PhraseStream.stream_chained(fQuartets)

		sample_quartet = {%TokenFreq{freq: 6, index: 1, is_common: false, offset: 3, token_id: <<0, 0, 0, 30>>},
		 				 [%TokenFreq{freq: 3, index: 4, is_common: false, offset: 6, token_id: <<0, 0, 0, 27>>}, 
		 				  %TokenFreq{freq: 2, index: 3, is_common: false, offset: 5, token_id: <<0, 0, 0, 28>>},
		  				  %TokenFreq{freq: 8, index: 2, is_common: false, offset: 4, token_id: <<0, 0, 0, 29>>}]}
			
		
		#phrase_id will be key to a tuple holding sentence_id and combinations - stored as indices which resolve to tokens via the token_map.
		#sentence id will be the key to getting hold of the token map which maps offset to token id


##this section could be re-usable in iterations.


		new_combination_map = Enum.reduce(quartet_stream, %{}, fn({key_type, colloc_types} = quartet, comb_map) ->			
			#IO.inspect(quartet)
			
			#IO.puts("Sentence_id: #{sentence_id}")
			collocs_len = length(colloc_types)
	
			#quartet_id = QuartetCounter.get_quartet_id()

			#Quartets.new(quartet_id, {sentence_id, quartet})
			
			#from each colloc we want {bin_tokens, sentence_id, token_offset} to match data structure for tokens/sentences
			#store quartet

			#IO.inspect(index_map)

			collocs = CollocStream.get_colloc_stream(quartet, index_map)

			offset_combinations_map = Enum.reduce(collocs, comb_map, fn({first_off, _last_off, colloc} = phrase, offset_combinations_map_accum) ->
				#%TokenInput{token: token, instance: %TokenInstance{sentence_id: sentence_id, offset: offset}}}, _from, {%WFL_Data{} = wfl_data, parent_wfl_pid} = state) do
				WFL.addToken(colloc_wfl_pid, %TokenInput{token: colloc, instance: %TokenInstance{sentence_id: sentence_id, offset: first_off}})  #check if first off references sentence or phrase - we should have sentence here
				#should we add last offset in with first offset as in offset: {first, last}

				# now add to this combination map
				# the point of this map is so that when the combination frequencies have been calculated we can find which sibling phrases are
					# continuations of the current one.
					# so we need an iterable something that lets us go through phrase sets which have combination linked with all other permutations for that phrase in the map
					# so i need to say for each combination, find continuation
					# each map links to more than one combination - there is a map per phrase
					# what does the map data look like - it is keyed on first offset and data is (an array) of combinations (see sample collcations below) - are these in index form or token_ids?  I think the latter - the former would require the token map to  hang around

					#IO.inspect(phrase)
					
					# phrase example = {23, 25, <<1, 0, 0, 11, 0, 0, 0, 12>>}
					phrase_id = PhraseCounter.get_phrase_id()
					Phrases.new(phrase_id, {sentence_id, phrase})

					# here put the accumulator for offset_combinations_accum - which is the offset version of the phrase combination keyed on the first offset
					# get the existing value for first_off in offset_combinations_map_accum

					offset_combinations = Map.get(offset_combinations_map_accum, first_off, [])
					new_offset_combinations = [colloc | offset_combinations]					
					Map.put(offset_combinations_map_accum, first_off, new_offset_combinations)					
			end)

			
			sample_collocations = [{<<"first_off">>, <<"last_off">>, <<1, 0, 0, 130, 2, 0, 0, 120, 0, 0, 0, 109>>},
								   {nil, nil, <<2, 0, 0, 130, 1, 0, 0, 112, 0, 0, 0, 109>>},
								   {nil, nil, <<1, 0, 0, 130, 0, 0, 0, 120, 1, 0, 0, 112, 0, 0, 0, 109>>},
								   {nil, nil, <<1, 0, 0, 130, 0, 0, 0, 120, 0, 0, 0, 112>>}, 
								   {nil, nil, <<2, 0, 0, 130, 0, 0, 0, 112>>},
								   {nil, nil, <<1, 0, 0, 130, 0, 0, 0, 120>>}]


			#{23, [25, 26, 28]} - I want to store this now along with token_map - here 23 would be key and [25, 26, 28] would be the data.

			#Map.put(comb_map, key_type, colloc_types)	#this should be last statement of the reduce function as it is the accumulator - may need more in the accumulator
			offset_combinations_map
		end)

		new_tokens_binary = update_in(tb.offset_maps.combination_map, fn(_old_comb_map) -> 
			new_combination_map
		end)

		#IO.inspect({sentence_id, new_combination_map})

		#we now need to save new_tokens_binary in tokensbinary agent
		TokensBinary.update(sentence_id, new_tokens_binary)


#	%TokenFreq{freq: 5, index: 2, token_id: <<0, 0, 0, 2>>},

		#IO.inspect(quartets)

		#WE STILL NEED to expand quartets as they are in {a, [b,c,d]} form at the moment.

		#now add all these to a wfl -colloc_wfl_pid.
		#before we do that we need to mark phrase up for abstractions - in this case a is concretisation for all comb([b,c,d])

		#set_bits(bit_list, merged_token)

	end

	def do_phrase(x,  colloc_wfl_pid) do 
		#{phrase_id, {sentence_id, {first_offset, last_offset, <<phrase token ids>>}}}
		#{234, {14, {2, 4, <<0, 0, 0, 125, 0, 0, 0, 30, 0, 0, 0, 29>>}}}	- use a struct so we can see what is going on?
		#{234, {20, {0, 1, <<0, 0, 0, 167, 0, 0, 0, 93>>}}}
		#IO.inspect(x)
		{_phrase_id, {sentence_id, {first_offset, last_offset, phrase_token_ids}}} = x

		phrase_freq = WFL.get_token_info(colloc_wfl_pid, phrase_token_ids)
		if phrase_freq.freq > 1 do
		
		
			# - objective is to see if there are any other sub-phrases that come from the same stable as this one that might adjoin and so make larger phrases
			# so we need to get hold of all other items with the same combination id - which we don't have - what we do have is token map keyed on offset
			# which is stored in TokenBinary keyed on sentence_id.  but that is not a combination list - is it a token map or a combination map - we need the latter.
			# one combination looks like this : {23, [25, 26, 28]} => {Sent_n, Off_a, Off_b, Off_c}
			# when we get x - a combination instance - we need to get other combination instances that start with Off_c + 1 or Off_c + 2 (here 29 or 30)
			# so given an offset number we need to retrive a combination for that and to then check if any of those instances occur frequently enough.
			# so offset plus sentence id yields a quartet.

			%TokensBinary{offset_maps: %OffsetMaps{token_map: index_map,  combination_map: combination_map}} = TokensBinary.get(sentence_id)

			continuations = Map.get(combination_map, last_offset)

			_sample_continuations = [[5, 4], [7, 4], [7, 5, 4]]  #no
			#so we have phrase token ids which will prepend combinations of the continuation which will be of up to 3 offsets

			#phrase_token_ids is what we are appending to - not sure if we need this as an offset or not - what would it be an offset into?
			
			#expand continuation and append to tokens of current phrase.


			###this part could be re-usable with say_hello

			#########################################################

			#IO.inspect({last_offset, continuation})
			_sample_index_map = %{11 => <<0, 0, 0, 7>>, 26 => <<0, 0, 0, 11>>, 15 => <<0, 0, 0, 20>>,
								  20 => <<0, 0, 0, 7>>, 17 => <<0, 0, 0, 18>>, 25 => <<0, 0, 0, 12>>,
								  13 => <<0, 0, 0, 22>>, 0 => <<0, 0, 0, 32>>, 8 => <<0, 0, 0, 25>>,
								  7 => <<0, 0, 0, 26>>, 1 => <<0, 0, 0, 7>>, 32 => <<0, 0, 0, 5>>,
								  3 => <<0, 0, 0, 30>>, 6 => <<0, 0, 0, 27>>, 2 => <<0, 0, 0, 31>>,
								  10 => <<0, 0, 0, 8>>, 9 => <<0, 0, 0, 24>>, 19 => <<0, 0, 0, 16>>,
								  14 => <<0, 0, 0, 21>>, 5 => <<0, 0, 0, 28>>, 18 => <<0, 0, 0, 17>>,
								  31 => <<0, 0, 0, 6>>, 22 => <<0, 0, 0, 14>>, 29 => <<0, 0, 0, 8>>,
								  21 => <<0, 0, 0, 15>>, 27 => <<0, 0, 0, 10>>, 24 => <<0, 0, 0, 13>>,
								  30 => <<0, 0, 0, 7>>, 23 => <<0, 0, 0, 11>>, 28 => <<0, 0, 0, 9>>,
								  16 => <<0, 0, 0, 19>>, 4 => <<0, 0, 0, 29>>, 12 => <<0, 0, 0, 23>>}

		#we need to add this phrase to the index map - or do we?  what are we binding with - concrete instances of phrases starting with a given offset.
		#we don't need to do anything other than glue the two together - no need to do any more combinations - that has been done
		#what we are gluing is two phrases - we look up what the second phrase is using the offset map.  having glued the two together, add to the wfl and press on.
		#the only thing that might be useful is to know the depth of this token or its parent parts so ab derives from a and b hmmm.

		#combined_phrase = << phrase_token_ids <> 
		if ! is_nil(continuations) do
			if sentence_id == 12 do
				IO.puts("Hello")
					#IO.inspect(continuations)
				#right now continuation looks like this:  [<<1, 0, 0, 129, 0, 0, 0, 29>>] 	(combinations which we don't have at the moment, but could looks like [[5, 4], [7, 4], [7, 5, 4]])
				phrase_candidates = List.foldl(continuations, [], fn(continuation, accum) -> 
					
					<< _overlap :: binary-size(4), phrase_extension :: binary >> = continuation
					phrase_candidate = phrase_token_ids <> phrase_extension
					[phrase_candidate | accum]
				end)

				Enum.each(phrase_candidates, fn(x) ->
					IO.inspect(x)
					#add to wfl
					#make sure we can re-iterate over extended phrases.  this means having access to the same continuations map
					#and having this token in the tokens map with its last_offset set correctly.  this will be the last offset of the continuation
					#when we iterate again we will need a specific list of phrases to iterate through
				end)

				# we need to translate offsets into tokens such that we preserve spaces in 4th binary
				#continuation = CollocStream.get_colloc(combination, token_map) do # perhaps should look at stream get_colloc_stream

				# then store new token in wfl
				# then we will need to be able to iterate again to extend phrase length where necessary.




			
			##########################################################
			end
			#IO.inspect(phrase_freq)
			_sample_phrase_freq = %WFL_Type{concretisations: %MapSet{}, freq: 2, instances: [{17, 2}, {12, 2}], is_common: false, type: <<0, 0, 0, 130, 0, 0, 0, 129>>, type_id: <<0, 0, 0, 249>>}
		end
	end
		
:ok
	end

	def get_phrases(bin_tok_freq_list, cutoff) do		
		get_phrases(bin_tok_freq_list, cutoff, 0, [], [])
	end

	def get_phrases([], _cutoff, gap_count, phrase, phrases) do
		new_phrase = Enum.drop(phrase, gap_count)
		add_phrase(new_phrase, phrases)		
	end

	def get_phrases(bin_tok_freq_list, cutoff, gap_count, phrase, phrases) when gap_count > 1 do #ideally want to pull max_gap_count out of config
		new_phrase = Enum.drop(phrase, gap_count)
		new_phrases = add_phrase(new_phrase, phrases)
		get_phrases(bin_tok_freq_list, cutoff, 0, [], new_phrases)
	end

	def get_phrases([tok_freq | rest], cutoff, gap_count, phrase, phrases) do
		{new_phrase, new_gap_count}  = if tok_freq.freq < cutoff || tok_freq.is_common == true do
			{phrase, gap_count + 1}
		else
			{[tok_freq.offset | phrase], 0}
		end	

		#need to keep track of definite article instances - perhaps and as well an pronouns but?- so that we can see if they concretise the phrase
		#we would need to stick this onto an accumulator associated with the phrase.
		get_phrases(rest, cutoff, new_gap_count, new_phrase, phrases)
	end

	def add_phrase(phrase, phrases) do		
		if length(phrase) > 1 do
			[Enum.reverse(phrase) | phrases]
		else
			phrases
		end
	end

	def merge_pairs([], accum) do
		accum
	end

	def merge_pairs([{%TokenFreq{} = tf_a, %TokenFreq{} = tf_b} | t], accum) do
		
		sample = {
			%TokenFreq{freq: 5, index: 2, token_id: <<0, 0, 0, 2>>},
	  		%TokenFreq{freq: 2, index: 3, token_id: <<0, 0, 0, 1>>}
  		}

  		a_ndx = tf_a.index
  		a_len = round(byte_size(tf_a.token_id) / 4)
  		b_ndx = tf_b.index
  		b_len = round(byte_size(tf_b.token_id) / 4)

  		overlap = a_ndx + a_len - b_ndx

  		merged_token = merge_pair(tf_a.token_id, tf_b.token_id, overlap)

  		abstractions = get_abstractions(merged_token)
  	
  		new_accum = [%TokenAbstractions{token: merged_token, abstractions: abstractions} | accum]

		merge_pairs(t, new_accum)  		

	end

	
	def merge_pair(tok_a, tok_b, overlap) when overlap <= 0 do
		#we need abs(overlap) gaps between a and b
		gap_bytes = abs(overlap) * 4
		tok_a <> <<0x00 :: integer-unit(8)-size(gap_bytes)>> <> tok_b		
	end

	def merge_pair(tok_a, tok_b, overlap) when overlap > 0 do
		IO.puts("overlap: #{overlap}")
		n_bytes = 4 * overlap
		<<_h :: binary-size(n_bytes),  rhs :: binary>> = tok_b
		tok_a <> rhs
	end

	
	#i want to go through bin_tok_freq_list a,b   b, c  :  c,d etc until a pair is unobtainable
	
	def get_pairs(bin_tok_freq_list, cutoff) do		
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


	def get_abstractions(merged_token) do
		combinations = get_set_bits(merged_token, 0, [])
			|> get_abstraction_tree()
	end

	def set_bits([], abstract_token) do
		#set bits replaces flagged tokens with a blank <0,0,0,0> token
		abstract_token
	end

	def set_bits([index | indices], abstract_token) do
		start = index * 4
		<< left :: binary-size(start), _token_id :: binary-size(4), rest :: binary >> = abstract_token
		new_token = left <> <<0 :: integer-unit(8)-size(4)>> <> rest

		set_bits(indices, new_token)
	end

	def get_set_bits(<<>>, _index, accum) do
		accum
	end

	def get_set_bits(<< left :: integer-unit(8)-size(4), rest :: binary >>, index, accum) do
		new_accum = if left >  0 do
			[index | accum]
		else
			accum
		end
		get_set_bits(rest, index + 1, new_accum)
	end


	def combine_list(list) do
		combine_list(list, [[]], [], [])		
	end

	def combine_list(list, seed) do
		combine_list(list, [seed], [], seed)		
	end

	def combine_list([], _with, accum, _seed) do
		#include the empty set for completeness
		#[[] | accum]
		#actually don't - caller can add it - or if the client knows it is at the front of the list it can always remove the head []
		accum
	end

	def combine_list([next | rest], with, accum, seed) do
		cutoff = get_cutoff()
		new_list = Enum.reduce(with, accum, fn([h | _t] = list, accum2) ->
			#IO.puts("h: #{h}    next: #{next}")	
			if (next - h - 1) > cutoff do
				accum2
			else
				[[next | list] | accum2]
			end		
		end)
		combine_list(rest, [seed | new_list], new_list, seed)
	end

	def remove_one([], _carousel, accum) do
		accum
	end

	def remove_one([index | indices], [h | t], accum) do		
		new_accum = [t | accum]		
		remove_one(indices, t ++ [h], new_accum)
	end

	#%TokenFreq{freq: 1, index: 2, is_common: false, offset: 37, token_id: <<0, 0, 0, 145>>},

	def get_abstraction_tree(token_freq_list) do		
		tree_sample = 
		[
			{
				[1, 2, 3],
		  		[
		  			{
		  				[2, 3], 
		  				[
		  					{[3], []}, 
		  					{[2], []}
		  				]
		  			}, 
		  			{
		  				[3, 1], 
						[
							{[1], []}, 
							{[3], []}
						]
					},
		   			{
		   				[1, 2], 
		   				[
		   					{[2], []}, 
		   					{[1], []}
		   				]
		   			}
		   		]
		   	}
		]

		remove_one_by_one([token_freq_list], [])
	end

	def remove_one_by_one([], accum) do
		accum
	end

	def remove_one_by_one([concretiser | t], accum) do
		one_less = remove_one(concretiser, concretiser, [])
		#this one_less list is associated with the concretiser		
		children = remove_one_by_one(one_less, [])
		#IO.puts("length: #{length(concretiser)}")	
		#could trap if there are no concretiser and just add [] to the accum
		#new_accum = [{concretiser, children} | accum]
		new_accum = if length(concretiser) == 0 do
			[]
		else
			[{concretiser, children} | accum]
		end

		remove_one_by_one(t, new_accum)		
	end

	def get_cutoff do
		2
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

defmodule CollocStream do
	def get_colloc_stream(quartet, token_map) do
	    Stream.resource(
	      fn ->
	      	{key_type, colloc_types} = quartet
	      	
	      	combinations = Collocation.combine_list(colloc_types, [key_type])
	      	
			{combinations, token_map}
	      end,
	      fn({combinations, token_map}) -> 
	        case combinations do 	#return next quartet as a token binary.  {:halt, accumulator} when finished.        	
	        	[] -> {:halt, []}
	        	[combination | rest] ->
	            	colloc = get_colloc(combination, token_map)
	            	{[colloc], {rest, token_map}}

	        end
	      end,
	      fn(empty_combination_list) -> empty_combination_list end 	#tidy up - returns the final value of the stream if there is one.
	    )  
  	end

	def get_colloc(combination, token_map) do
		#IO.inspect(combination)
		#IO.inspect(token_map)
		get_colloc(combination,  0, token_map, {nil, nil, <<>>})
	end

	def get_colloc([], _prev_offset, _token_map, acc) do
		acc
	end

	def get_colloc([index | indices], prev_offset, token_map, {first_offset, last_offset, colloc}) do
		IO.puts("index: #{index}")
		tok_freq = Map.get(token_map, index)

		new_gap = case prev_offset do
			0 ->
				prev_offset
			_ ->
				prev_offset - tok_freq.offset - 1
		end

		last_off = case colloc do
			<<>> -> 
				tok_freq.offset
			_ ->
				last_offset
		end

		first_off = case indices do
			[] -> 
				tok_freq.offset
			_ ->
				first_offset
		end

		
		#shift_new_gap = new_gap * round(:math.pow(2, 6))	 #or left shift 6 times  - use shift_new for very large corpora where we need 4th byte for colloc ids

		<<token_id :: binary-size(1),  rest :: binary-size(3)>> = tok_freq.token_id
		new_token_int = :binary.decode_unsigned(token_id) + new_gap
		new_token_id = <<new_token_int :: integer-unit(8)-size(1)>> <> <<rest :: binary >>
		
		new_colloc = << new_token_id <> colloc >>	

		#{first_off, last_off, <<2, 0, 0, 130, 1, 0, 0, 112, 0, 0, 0, 109>>},

		get_colloc(indices, tok_freq.offset, token_map, {first_off, last_off, new_colloc})
	end

	def quartet_combination(3) do 
		[[3, 1, 0], [3, 2, 0], [3, 2, 1, 0], [3, 0], [2, 1, 0], [2, 0], [1, 0]]
	end

	def quartet_combination(2) do 
		[[2, 1, 0], [2, 0], [1, 0]]
	end

	def quartet_combination(1) do 
		[[1, 0]]
	end
end


defmodule PhraseStream do
  
  def get_quartets([]) do  #doubt we come here
		#quartets are rolling sequences of 4 - Enum.chunk does something simiar but does not do the tail where the chunk length tapers to 1
		#a quartet may not have four members  it has up to 4 members and at least 2
		[]
	end

	def get_quartets(token_freq_list) do
		get_quartets(token_freq_list, [])
	end

	def get_quartets([_ | []], quartets) do		
		#quartet needs at least 2 members
		quartets
	end

	def get_quartets([a | bcd_etc], quartets) do
		dcb = get_bcd(bcd_etc, [])
		bcd = Enum.reverse(dcb)	#may not need to do this - we could store <<on hand other the>> instead of <<on the other hand>>
		new_quartets = [{a, bcd} | quartets]
		get_quartets(bcd_etc, new_quartets)
	end


	def get_bcd([], bcd) do
		bcd		
	end

	def get_bcd(_, [_, _, _] = bcd) do
		bcd	
	end

	def get_bcd([h | t], bcd) do
		new_bcd = [h | bcd]
		get_bcd(t, new_bcd)
	end

  def get_phrase_stream(phrases_list) do
    Stream.resource(
      fn ->
      	phrases_list #array per sentence each with array of phrases.
      end,	#this fn intitialises the resource - it takes no params and returns 'the resource' 
      fn(remaining_phrases_list) -> 
        case remaining_phrases_list do 	#return next wfl_item.  {:halt, accumulator} when finished.        
        	[] -> {:halt, []}
        	[phrase | remaining_phrases] ->  

            	{[phrase], remaining_phrases}
        end
      end,
      fn(empty_phrases_list) -> empty_phrases_list end 	#tidy up - returns the final value of the stream if there is one.
    )
  end  

	def unchained(enumerable) do
		fn() ->  			
			case enumerable do  				
				[h | []] ->
					{h, unchained(nil)}
				[h | t] ->
					{h, unchained(t)}
				_ ->
					{nil, unchained(nil)}
			end  			 	
		end
	end

	def chained(fCx) do  		
		chained(unchained(nil), fCx)
	end

	def chained(fData, fCx) do  				
		fn() ->			
			{data, fData2} = fData.()	#see if we have data of our own - fData2 is continuation func holding remainder of the list after data has been stripped from the head
			{nextData, next_fData, next_fCx} = case data do
				nil ->
					#we have no data of our own, so get context from parent, and extract that
					{newDataFunc, newCxFunc} = fCx.()	#data2 is the new context (expected to be a list), and fCx2 the continuation function for the remainder of list of lists.
					{newData, newNextDataFunc} = newDataFunc.()
					
					{newData, newNextDataFunc, newCxFunc}
				_ ->
					{data, fData2, fCx}
			end
			{nextData, chained(next_fData, next_fCx)}
		end
	end

	def quartet_extractor(fPhraseIterator) do
		fn() ->
			{phrase,  fNextPhraseIterator} = fPhraseIterator.()		
			
			if is_nil(phrase) do
				nilFunc = unchained(nil)
				{nilFunc, nilFunc}
			else
				r_phrase = Enum.reverse(phrase) #might be able to work backwards through the list without this reverse, but for now easier to read output if forwards
				quartets = get_quartets(r_phrase)
				qUnchained = unchained(quartets)					
				nextExtractor = quartet_extractor(fNextPhraseIterator)
				{qUnchained, nextExtractor}
			end			
		end
	end
	

	def stream_chained(fChain) do
		Stream.resource(
		  fn ->
		  	fChain
		  end,	
		  fn(f_chain) ->
		  	{v, f} = f_chain.()    	
		      	if is_nil(v) do 
		      		{:halt, f}
		      	else 
		      		{[v], f}
		      	end    
		  	
		  end,
		  fn(chain) -> chain end
		)
	end
end