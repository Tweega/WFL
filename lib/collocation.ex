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
		cutoff = get_cutoff()

		#get freqs for each token_id - TokenStream
		token_stream = TokenStream.get_token_stream(sent_bin_tokens.bin_tokens)	#using a stream to hand out token_ids into 4 byte chunks
		_token_count = div(byte_size(sent_bin_tokens.bin_tokens), 4)

		#get freqs for each token_id - map
		bin_tok_freq_list = Enum.map(token_stream, fn(tok_id) ->
			#get the wfl_info for this token
			wfl_info = WFL.get_token_info_from_id(source_wfl_pid, tok_id)
			%TokenFreq{token_id: tok_id, freq: wfl_info.freq, is_common: wfl_info.is_common}
		end)
		
		#split this list on sequences of tokens with freq > cutoff or where the gap is less than 3 ignore phrases of length 1 only
		phrases = get_phrases(bin_tok_freq_list, cutoff)
		###IO.inspect(phrases)
		#bin_tok_freq_list
		sample_phrases = [%TokenFreq{freq: 9, index: 1, is_common: false, offset: 8,  token_id: <<0, 0, 0, 42>>},
						  %TokenFreq{freq: 4, index: 2, is_common: false, offset: 9,  token_id: <<0, 0, 0, 125>>},
  						  %TokenFreq{freq: 2, index: 4, is_common: false, offset: 11, token_id: <<0, 0, 0, 121>>}]

		#--chained(fData, fCx) fData is func for quartets which will initially return nil.  
		# fCx is called on data is nil and returns a new data item and data continuation func and cx cont func

		nilFunc = PhraseStream.unchained(nil)	#this is initial setting for quartets - data
		fPhrase = PhraseStream.unchained(phrases)
		fQuartetExtractor = PhraseStream.quartet_extractor(fPhrase)

		fQuartets = PhraseStream.chained(nilFunc, fQuartetExtractor)

		#quartets = PhraseStream.chained(phrase_stream)
		quartet_stream = PhraseStream.stream_chained(fQuartets) #- looks like we don't need this after all, though we could modify how we extract data for quartet
		Enum.each(quartet_stream, fn(i) -> IO.inspect(i) end)

		#IO.inspect(quartets)

		#WE STILL NEED to expand quartets as they are in {a, [b,c,d]} form at the moment.

		#now add all these to a wfl -colloc_wfl_pid.
		#before we do that we need to mark phrase up for abstractions - in this case a is concretisation for all comb([b,c,d])

		#set_bits(bit_list, merged_token)
	end


	def get_phrases(bin_tok_freq_list, cutoff) do		
		get_phrases(bin_tok_freq_list, cutoff, 0, [], [], 0, 0)
	end

	def get_phrases([], _cutoff, gap_count, phrase, phrases, _index, _offset) do
		new_phrase = Enum.drop(phrase, gap_count)
		add_phrase(new_phrase, phrases)		
	end

	def get_phrases(bin_tok_freq_list, cutoff, gap_count, phrase, phrases, _index, offset) when gap_count > 1 do #ideally want to pull max_gap_count out of config
		new_phrase = Enum.drop(phrase, gap_count)
		new_phrases = add_phrase(new_phrase, phrases)
		get_phrases(bin_tok_freq_list, cutoff, 0, [], new_phrases, 0, offset)
	end

	def get_phrases([tok_freq | rest], cutoff, gap_count, phrase, phrases, index, offset) do
		{new_phrase, new_gap_count}  = if tok_freq.freq < cutoff || tok_freq.is_common == true do
			{phrase, gap_count + 1}
		else
			{[%TokenFreq{tok_freq | index: index, offset: offset} | phrase], 0}
		end	

		#need to keep track of definite article instances - perhaps and as well an pronouns but?- so that we can see if they concretise the phrase
		#we would need to stick this onto an accumulator associated with the phrase.
		get_phrases(rest, cutoff, new_gap_count, new_phrase, phrases, index + 1, offset  + 1)
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

	#WORKING HERE - don't want to merge pairs only but work instead on the whole phrases - or sections of it that meet proximity rules
	#abstractions will be derived in get_abstraction_tree
	#so we want an array of phrases from the sentence where phrases are demarkated by frequent types not more than 2 spaces apart. 

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
		combine_list(list, [[]], [])		
	end

	def combine_list([], _with, accum) do
		#include the empty set for completeness
		#[[] | accum]
		#actually don't - caller can add it - or if the client knows it is at the front of the list it can always remove the head []
		accum
	end

	def combine_list([next | rest], with, accum) do
		new_list = Enum.reduce(with, accum, fn(list, accum2) ->
			[[next | list] | accum2]		
		end)
		combine_list(rest, [[] | new_list], new_list)
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
		bcd = get_bcd(bcd_etc, [])
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
				quartets = get_quartets(phrase)
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