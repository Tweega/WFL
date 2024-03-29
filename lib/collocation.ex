#Collocation module is a collection of functions only called by WFL_Server and so is essentially part of that module
# and always runs in the WFL Server process

#	Note on gaps.  Phrases are stored as 2 sets of 4 bytes (LHS and RHS), the first of which stores the number of spaces (this may be reduced to smaller number of bits)
#	the gap integer is stored on the RHS.  LHS in turn is either a key to another <<LHS, RHS>> pair or an actual token, such as "cat".

# defmodule TokenInput do
# 		defstruct([:token, :instance])
# 	end
#
#
# defmodule TokenInstance do
# 		defstruct([:sentence_id, :offset])
# 	end
#
defmodule TokenFreq do
	defstruct([token_id: <<>>, freq: 0, index: -1, offset: -1, is_common: false])
end

defmodule TokenAbstractions do
	defstruct([token: <<>>, abstractions: [] ])
end


defmodule Collocation do
	require Logger

	defstruct([token_ids: <<>>, concretisations: []])	#anything that further defines a pattern is a concretisation - though a shorter word would be nice.  ab is concretisation of a and b as is a_b.  acb is concretisation of a_b


	def check_wfl(wfl_pid) do
		x = WFLScratch.Server.get_sorted_wfl(wfl_pid, :freq, :desc)
		IO.inspect(x)
	end


	#for each sentence, for each continuation list - extend the continuation list and recurse until no more continuations.

	def process_sent_map_obsolete(sent_map, continuation_wfl_pid, parent_wfl_pid, sent_x_fun) do 	#sent_x_fun needed because initial sentence map will be different to subsequent ones - need to ratioalise tokensbinary
		#don't think we need the continuation_wfl_pid
		IO.inspect("start new wfl")
		sents = Stream.map(sent_map, fn({sentence_id, _}) -> sentence_id end)
		{:ok, phrase_wfl_pid} = WFL.start_link(parent_wfl_pid)
		Parallel.pjob(sents, [{Collocation, :x_phrases, [sent_map, phrase_wfl_pid, sent_x_fun]}]) ##could we not use sent_map instead of sents? tk

		# we now have a wfl with collocation frequencies.  we want to do the loop again	for which we need a combination map
		# which is indexed on sentence, then continuations indexed on offset.
		#so we are going to call process_sent_map again
		{new_sent_map, freq_token_count} = create_sent_map_from_wfl(phrase_wfl_pid)

		##new_sent_x_fun = fn(x) -> x end

		##process_sent_map(sent_map, continuation_wfl_pid, colloc_wfl_pid, sent_x_fun)
		##IO.inspect(new_sent_map)
	end

	def create_sent_map_from_wfl(wfl_pid) do
		types = WFL.get_wfl(wfl_pid).types	#ideally this would be a stream tk.  can we reduce from a stream - i would have thought so.
		#cutoff = get_cutoff()
		#could we parallelise this? to do this we would have to replace the reduce mechanism wiht parallel_job - otherwise should be no problem. probably should fo the frequency filter before parralellising
		#anonymous fn({15, {3, 3}}, %{})
		#sent map tels me how to extend a phrase given sentence and last offset

		Enum.reduce(types, {%{}, 0}, fn({token_key, %WFL_Type{} = wfl_type}, {sent_map, freq_token_count}) ->
		#IO.inspect(sent_map)
			if wfl_type.freq > 1 do
#IO.inspect(wfl_type.instances)
				Enum.reduce(wfl_type.instances, {sent_map, freq_token_count}, fn({sent_id, {first_offset, last_offset}}, {sent_map_acc, tok_count}) ->
					#IO.inspect({sent_id, first_offset, last_offset})
					{_, new_sent_map} = Map.get_and_update(sent_map_acc, sent_id, fn(cm) ->
						continuation_map = case cm do
							nil -> %{}
							_ -> cm
						end


						{_, new_continuation_map} = Map.get_and_update(continuation_map, first_offset, fn(continuation_list) ->
							continuations = case continuation_list do
								nil -> []
								_ -> continuation_list
							end

							{nil, [{last_offset, wfl_type.type_id, token_key} | continuations]}

	    				end)

		    			{nil, new_continuation_map}

	    			end)
					{new_sent_map, tok_count + 1}
				end)
			else
				{sent_map, freq_token_count}
			end
		end)
	end


	def pre_pair_up({sent_id,  lhs_cont_map}, root_sent_map, colloc_wfl_pid, add_phrase_query) do
		{:ok, rhs_cont_map} = Map.fetch(root_sent_map, sent_id)
		pair_up(sent_id, lhs_cont_map, rhs_cont_map, colloc_wfl_pid, add_phrase_query)
	end

	def pair_up(sent_id, lhs_phrase_map, continuation_map, colloc_wfl_pid, add_phrase_query) do

	#continuation map value is an array because we may have phrase tuple of eg "the black", and "the _ cat"
	_sample_continuation_map = %{0 => [{0, <<0, 0, 0, 93>>, "we"}],
	    1 => [{1, <<0, 0, 0, 92>>, "needed"}],
	    2 => [{2, <<0, 0, 0, 7>>, "the"}],
	    3 => [{3, <<0, 0, 0, 33>>, "perfect"}],
	    4 => [{4, <<0, 0, 0, 88>>, "performance"}],
	    5 => [{5, <<0, 0, 0, 42>>, "and"}],
	    6 => [{6, <<0, 0, 0, 29>>, "it"}],
	    7 => [{7, <<0, 0, 0, 41>>, "was"}],
	    8 => [{8, <<0, 0, 0, 4>>, "one"}],
	    9 => [{9, <<0, 0, 0, 75>>, "of"}],
	    10 => [{10, <<0, 0, 0, 7>>, "the"}],
	    11 => [{11, <<0, 0, 0, 162>>, "best"}],
	    12 => [{12, <<0, 0, 0, 93>>, "we"}],
	    15 => [{15, <<0, 0, 0, 8>>, "in"}]}

		cutoff = 2  	#+ 1?
		# we don't need to reduce as we only want to store in wfl.  Should be able to replace with each loops and can just return :ok here.
		# however, placing the WFL.addToken code inside the reduce makes it blow up for some reason - so doing that in the pre pair_up function
		Enum.each(lhs_phrase_map, fn({lhs_first_off, lhs_phrases}) ->
			Enum.each(lhs_phrases, fn({lhs_last_off, lhs_token_id, _})->
				Enum.each(0..cutoff, fn (gap) ->
					rhs_first_off = lhs_last_off + gap + 1
					unless cutoff + 1 < rhs_first_off - lhs_last_off do
						case Map.fetch(continuation_map, rhs_first_off) do
							{:ok, rhs_continuations} ->
								Enum.each(rhs_continuations, fn({rhs_last_off, rhs_token_id, _})->
									#shift_new_gap = new_gap * round(:math.pow(2, 6))	 #or left shift 6 times  - use shift_new for very large corpora where we need 4th byte for colloc ids

									<<_gap_byte :: binary-size(1),  rhs_rest :: binary-size(3)>> = rhs_token_id
									#new_token_int = :binary.decode_unsigned(token_id) + new_gap 	#need to update this so that new_gap is two most sig bits of integer - this only works if token_id = 0
									new_rhs_token_id = <<gap :: integer-unit(8)-size(1)>> <> <<rhs_rest :: binary >>

									phrase_candidate = lhs_token_id <> new_rhs_token_id

									WFL.addPhrase(colloc_wfl_pid, %TokenInput{token: phrase_candidate, instance: %TokenInstance{sentence_id: sent_id, offset: {lhs_first_off, rhs_last_off}}}, add_phrase_query)
									#IO.inspect(phrase_candidate)

								end)
							_ ->
									#don't seem to be handling a failure return type
									#IO.puts("Not handling failure to retrieve value from continuation map.")
									#we do come here quite a lot.  Need to check if this is expected behaviour.
									nil
						end
					end
				end)
			end)
	 	end)
	end

	def temp_get_x(_sentence_map, sent_id) do
		#IO.inspect({:sent_id, sent_id})
		%TokensBinary{offset_maps: %OffsetMaps{token_map: _index_map,  combination_map: combination_map}} = TokensBinary.get(sent_id)

		combination_map
	end


	def set_continuations({_key, wfl_type}) do


		#this function adds continuation instances to the map of continuations stored for each sentence, indexed on offset
		#it takes in a wfl type and checks for frequency - if over cutoff then adds each instance to the respective combination maps.
		_sample_p_s = {<<0, 0, 0, 93, 0, 0, 0, 183, 0, 0, 0, 101>>,
			 %{concretisations: [], freq: 1, instances: [{19, {0, 2}}],
			  is_common: false, type: <<0, 0, 0, 93, 0, 0, 0, 183, 0, 0, 0, 101>>,
			  type_id: <<0, 0, 1, 42>>}}

		if wfl_type.freq > 1 do 	# is this check redundant?
			#occurrences for this wfl_type are listed in wfl_type.instances

			#for each instance of this wfl_type
				#get {Si, Oj}, add to list of continuations for that sent/offset

			instances = wfl_type.instances
			Enum.each(instances, fn({sent_id, {first_offset, last_offset}}) ->  #NOTE offsets may also have max_offset in 3-tuple.
				#get existing continuations for sent/offset
				%TokensBinary{offset_maps: %OffsetMaps{token_map: _index_map,  combination_map: combination_map}} = TokensBinary.get(sent_id)

				###- {max_off, offset_combinations} = Map.get(combination_map, first_offset)
				{max_off, offset_combinations} = case Map.get(combination_map, first_offset) do
					{max_offz, offset_combinationsz} ->
						{max_offz, offset_combinationsz}
					_ ->
						#IO.inspect({:a, first_offset, last_offset, combination_map})
						nil
				end
				#colloc_len = get_last_offset(wfl_type.type, 0)	# how would this not be last_offset - first_offset + 1? may want to do a test for this
				colloc_len = last_offset - first_offset + 1

				new_offset_combinations = [{wfl_type.type_id, colloc_len} | offset_combinations]
				Map.put(combination_map, first_offset, {max_off, new_offset_combinations})

			end)
		end
		:ok
	end

	def do_concretisation({_key, wfl_type}, _root_wfl_pid, _last_wfl_pid, _deadend_wfl_pid) do

		#if this is a frequent phrase, then addd to root wfl.  Note that we don't know how far down the phrase chain we are which may affect cutoff TK
		_cutoff = get_cutoff()
		if wfl_type.freq > 1 do
			## each phrase is concretisation of the set of phrases that is itself minus one token cat sat on -> {cat, [saton, _on sat]}
			#get a stream of this phrase minus one token - what form do we have the phrase in - presumably token ids
			expansion = 1
			###abstractions = lose_one_bin(wfl_type)

			#for each abstraction - check deadend

			#then check freq in wfl - need to be able to search up the wfl tree
			#if over c/o add to concretisations in wfl - otherwise add to dead_end

		#else
			#otherwise we can delete the node - though possibly that does not matter as we will let go of the whole wfl.
		end

	end

	def expand_phrases() do
		#get list of all colloc wfls
		#we are going to go through these in order from smallest to largest, look up smallest and stick on the rhs.
		#having a bit of trouble with this.
		#parent_wfl_pid = WFL.get_parent(last_wfl_pid)
		wfl_chain = X_WFL.get_wfl_chain()
		{wfl_pid, colloc_pid, colloc_chain} = case wfl_chain do
			[root_wfl_pid, root_colloc_pid | rest_chain] -> {root_wfl_pid, root_colloc_pid, rest_chain}
			_ -> {nil, []}
		end
		unless is_nil(colloc_pid) do
			#for each frequent phrase in the root colloc (token pairs) add to concretisation dictionary
			#for the incremental approach to concretisation, we are going to have to be able to look up concretisation by token id
			#start Expansion gen_server
			{:ok, pid} = Expansion.start_link(wfl_pid, colloc_pid)

			exp_phrases(colloc_chain)
		end

		{wfl_pid, colloc_pid, colloc_chain}
	end

	def exp_phrases([]) do

	end

	def exp_phrases([colloc_pid | rest_pids]) do
		#for each type in colloc_pid wfl with freq > c/o
		#get lhs/rhs - look up lhs in parent, and append to rhs
		#we actually need to look up lhs in Expansion, and if it is not there then we have the root colloc wfl, in which case we look there

		{wfl, parent_pid} = WFL.get_wfl_state(colloc_pid)
		#grandparent_pid = WFL.get_parent(parent_pid)
		#IO.puts("here we are")
		Enum.each(wfl.types, fn({token_key, %WFL_Type{} = wfl_type})  ->
			if wfl_type.freq > 1 do
				<<lhs :: binary-size(4),  rhs :: binary-size(4)>> = token_key
				#the lhs should never have any spaces embedded in it

				expansion = Expansion.get_phrase(lhs)
				lhs_phrase = if is_nil(expansion) do
					#lhs_phrase must be from root colloc
					{phrase, _grandparent_pid} = WFL.get_token_from_id(parent_pid, lhs)
					phrase
				else
					expansion
				end

				xp_phrase = <<lhs_phrase :: binary, rhs :: binary>>
				#as we 'lose_one' from phrases to create abstractions, we want to be able to find the token_info for those abstractions,
				#but the abstractions are stored in compact form so we need to create new index for them
				#save this concretisation to the token_info
		####    defstruct([:type_id, concretisations: MapSet.new()])	#concretisations holds token_ids of types that extend the current type ie catsat extends cat and sat

				Expansion.new(xp_phrase, %ExpansionItem{:wfl_pid => colloc_pid, :phrase_id => wfl_type.type_id})

			end
		end)

		exp_phrases(rest_pids)

	end

	def check_expansions([]) do

	end

	def check_expansions([wfl_pid | rest_wfls]) do
		#the id is in phrase map
		#the expansion is in expansion map
		%WFL_Data{type_ids: type_ids} = WFL.get_wfl(wfl_pid)
		Enum.each(type_ids, fn({phrase_id, short_phrase}) ->
			token_info = WFL.get_token_info_from_id(wfl_pid, phrase_id)
			if token_info.freq > 1 do
				#we should be able to find phrase_id in Expansion.phrase_map
				z = Expansion.get_phrase(phrase_id)
				#IO.inspect({phrase_id, z})
				expanded_phrase = X_WFL.expand_type_id(wfl_pid, phrase_id, false)
				expanded_phrase2 = X_WFL.expand_type_id(wfl_pid, phrase_id, true)
				##IO.inspect({:xp, expanded_phrase2})
				phrase_id2 = Expansion.get_phrase_id(expanded_phrase)
				##IO.inspect({:phrase_id, phrase_id2})
			end

		end)
		check_expansions(rest_wfls)
	end

	defp do_concretise_phrases([wfl_pid | []]) do
IO.puts("That's all folks")
	end

	defp do_concretise_phrases([colloc_pid | rest_pids]) do
		cutoff = 2
		#we want to set up a stream here iterating through wfls
		#for the moment only, process a complete list of phrases from the last wfl
		phrases = WFL.get_wfl(colloc_pid).types
		frequent_phrases = Enum.filter(phrases, fn({_,  wfl_type}) ->
			_sample_colloc = {<<0, 0, 0, 93, 0, 0, 0, 183, 0, 0, 0, 101>>,
					 %{concretisations: [], freq: 1, instances: [{19, {0, 2}}],
					  is_common: false, type: <<0, 0, 0, 93, 0, 0, 0, 183, 0, 0, 0, 101>>,
					  type_id: <<0, 0, 1, 42>>}}

			wfl_type.freq >= cutoff
		end)

		case frequent_phrases do
			[_h | _t] = frequent_phrases ->
				#we have at least one frequent phrase so process it
				Parallel.pjob(frequent_phrases, [{Collocation, :concretise_phrase, [colloc_pid]}])
			_ ->
				colloc_pid
		end
		do_concretise_phrases(rest_pids)
	end

	def concretise_phrases() do
		#create a wfl to hold all the phrases that have already been processed.
		#process each phrase in the wfl

		##processed_phrases = ProcessedPhrases.start_link()	#start this in the supervisor?

		# loop over all the colloc wfls and need to consider
		wfl_chain =
			X_WFL.get_wfl_chain()
			|> Enum.reverse

		do_concretise_phrases(wfl_chain)

	end

	defp get_concretiser(wfl_pid, concretiser_id) do
		#if ABC only concretised by ABCD, then AB, AC, BC are concretised by ABCD, not by ABC"
		#concretiser is handed down until abstraction is more frequent than concretiser
		concretiser = WFL.get_token_info_from_id(wfl_pid, concretiser_id)
		#i think that x is now simply concretiser - how can that be?
		##IO.inspect({:root_info, concretiser.root_info})
		#if we have a root_info, use that, otherwise use this

		%RootInfo{freq: concretiser_freq, conc: %Concretisation{}} = concretiser.root_info
    #
		# IO.inspect({:root_info, x.root_info})
		# IO.inspect(x.type_id == concretiser)
		# IO.inspect(x)
		# IO.inspect({:root_info, x.root_info})
		# IO.inspect({:root_info, x.root_info})
		# IO.inspect(x.type_id == concretiser)
		# IO.inspect(x)
		# IO.inspect({:root_info, x.root_info})

		xx = case concretiser_freq do
			0 ->
				#use this type's info
				%RootInfo{freq: concretiser.freq,  conc: %Concretisation{pid: wfl_pid, token_id: concretiser.type_id}}
			_ ->
				concretiser.root_info
		end

		if xx.conc.token_id == <<0, 0, 3, 59>> do
			IO.inspect ({:phrase, X_WFL.expand_type_id(wfl_pid, concretiser_id, true), :root, xx.valid})
		end

		xx
	end

	def concretise_phrase(phrase, concretiser_pid) do
		{_key, type} = phrase


		#this looks like a blocking call - we are calling back into wflscratch server from a job started there, so neither job can now proceed.
		# the only way forward for the moment is to call the end function directly so that we stay on this thread.
		#later we need to review which modules have which functions
		expanded_phrase = X_WFL.expand_type_id(concretiser_pid, type.type_id, false)	#false leaves the expansion as binary token ids

		conc_space_count = get_space_count(expanded_phrase)
		#may not be so simple.  I think we are going to have to find a way not to start this exercise from inside WFLScratch.Server
		#IO.inspect({:expanded_phrase, expanded_phrase})

		#if ProcessedPhrases.contains(type.type_id) == false do
			abstractions = lose_one_bin(expanded_phrase)
			# IO.inspect(abstractions)
			#parent_wfl_pid = WFL.get_parent(concretiser_pid)

			root_info = get_concretiser(concretiser_pid, type.type_id)

			# if root_info.conc.token_id == <<0, 0, 3, 59>> do
			# 	IO.inspect({:concretiser, type.root_info.valid, type.type_id})
			# end

#IO.inspect({:root_info, root_info})
			make_concrete(abstractions, root_info, conc_space_count)
			#ProcessedPhrases.new(type.type_id)
		#else
			#IO.puts("Do we ever encounter a situation where a phrase has already been processed?")
		#end

	end


	def make_concrete([], _concretiser_info, _conc_space_count) do
	end

	def make_concrete([next_abstraction | rest_abstractions], concretiser_info, conc_space_count) do
		#here find the abstraction in its wfl and add phrase to its concretisation list.
		#problem here is that phrases are only ever two tokens long..a token for the phrase so far (lhs) and one for the additional token (rhs)
		#we cannot use the expanded token as the key... so how to find the entry so as to set its concretisations
		#we could do a series of searches - initially for the first 2 tokens, then for that token plus the third, and so on.
		#not excellent...but are there other options?
		#we are going to have to find the tokens at some point.
		#IO.inspect({:next_abstraction, next_abstraction})

		abstraction_space_count = get_space_count(next_abstraction)

		new_spaces = abstraction_space_count > conc_space_count

		size_abstraction = byte_size(next_abstraction)
		if size_abstraction > 0 do
			jj = Expansion.add_concretisation(next_abstraction, concretiser_info, new_spaces)
			#IO.inspect({:owner, next_abstraction, :conc, concretiser_id})

		else
			Logger.debug("How can next abstraction size be zero?")
			IO.inspect(next_abstraction)
		end
		make_concrete(rest_abstractions, concretiser_info, conc_space_count)
	end

	def get_space_count(token_bin) do
		get_space_count(token_bin, 0)
	end


	defp get_space_count(<<>>, space_count) do
		space_count
	end


	defp get_space_count(<<byte4 :: binary-size(4), rest :: binary>>, space_count) do
			<<count :: integer-unit(8)-size(1), _token_bytes :: binary>> = byte4
			get_space_count(rest, space_count + count)
	end


	def lose_one_bin(bin4) do
		#returns a list of binaries each one token shorter than the initial input binary

		bs = byte_size(bin4)
		if bs < 5 do 	#single token_ids otherwise abstract to [''] instead of []
			[]
		else
			lose_one_bin(bin4, <<>>, [])
		end
	end

	def lose_one_bin(<<>>, _bin2, acc) do
		acc
	end


	def lose_one_bin(<<byte4 :: binary-size(4), rest :: binary>>, bin2, acc) do
		size_bin = byte_size(bin2)
		size_rest = byte_size(rest)

		#space_count are spaces before the owning token.  i.e. I am so many spaces plus 'cat'
		#phrase-less-one - for all instances other than the first, when bin2 is empty, we need to add a space to the first of rest.
		#this additional space should only apply once - so should not be part of the rest that is passed on in recursion..

		#{new_bin, new_rest} =
		new_acc =
			if size_bin == 0 do
				#of ABCD rest = BCD - we have removed first token so make sure that second token has no leading spaces
				<<_space_count :: integer-unit(8)-size(1), rest_rest :: binary>> = rest	#rest may have a leading space - we have dropped the first token
				spaceless_token = <<0>> <> rest_rest
				#{bin2, spaceless_token} # bin2 is empty
				[<<spaceless_token :: binary >> | acc]
			else
				#bin2 will now have at least A in it, perhaps BA, or CBA, while rest will have  BC or C or empty
				rev_b = Utils.rev_bin4(bin2)
				if size_rest == 0 do
					#bin2 will have CBA
					#{rev_b, rest}
					[<< rev_b :: binary, rest :: binary >> | acc]
				else
					#rest has either D or CD while bin2 will have or AB or A so we will end up with either ABD or ACD
					#we have to make ssure that for ACD the spacing for C is correct, and for ABD that the spacing for D is correct
					# specifically for <<0, 0, 0, 7,      1, 0, 0, 129,     0, 0, 0, 130,     0, 0, 0, 129>>
					# when we have ACD, or <<0, 0, 0, 7,           0, 0, 0, 130,     0, 0, 0, 129>>
					# we end up with <<0, 0, 0, 7,           2, 0, 0, 130,     0, 0, 0, 129>>
					# and for ABD, <<0, 0, 0, 7,      1, 0, 0, 129,        0, 0, 0, 129>>
					#we end up with <<0, 0, 0, 7,      1, 0, 0, 129,       1, 0, 0, 129>>
					# we need to get hold of the token that we are losing (B or C) and inspect the spaces on that - ie look at byte4

					<<rest_spaces :: integer-unit(8)-size(1), rest2 :: binary>> = rest
					<<b4_spaces :: integer-unit(8)-size(1), _ :: binary>> = byte4
					space_count = rest_spaces + b4_spaces
					#if space count is now too high, return an empty abstraction
					if space_count > 1 do
						#{<<>>, <<>>}
						acc
					else
						spaced_rest = << space_count + 1 >> <> rest2
						#{rev_b, spaced_rest}
						[<< rev_b :: binary, spaced_rest :: binary >> | acc]
					end

				end
			end

		#new_acc = [<< new_bin :: binary, new_rest :: binary >> | acc]
		lose_one_bin(rest, << byte4 :: binary, <<bin2 :: binary>> >>, new_acc)
	end



	def get_last_offset(phrase_extension, len) do
		colloc_length(phrase_extension, len)
	end

	def colloc_length(<<>>, len) do
		len
	end

	def colloc_length(<< gap_count :: integer-unit(8)-size(1), _token_id :: binary-size(3), rest :: binary >>, len) do
		colloc_length(rest, len + gap_count + 1)
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

		_sample = {
			%TokenFreq{freq: 5, index: 2, token_id: <<0, 0, 0, 2>>},
	  		%TokenFreq{freq: 2, index: 3, token_id: <<0, 0, 0, 1>>}
  		}

  		a_ndx = tf_a.index
  		a_len = round(byte_size(tf_a.token_id) / 4)
  		b_ndx = tf_b.index
  		###b_len = round(byte_size(tf_b.token_id) / 4)

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


	def get_abstractions(merged_token) do
		_combinations = get_set_bits(merged_token, 0, [])
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

	#%TokenFreq{freq: 1, index: 2, is_common: false, offset: 37, token_id: <<0, 0, 0, 145>>},

	def get_abstraction_tree(token_freq_list) do
		_tree_sample =
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

	def remove_one([], _carousel, accum) do
		accum
	end

	def remove_one([_index | indices], [h | t], accum) do
		new_accum = [t | accum]
		remove_one(indices, t ++ [h], new_accum)
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
	      	#this quartet results in a sequence of combinations
	      	#for a, [b, c, d] - if we end up only with a, b, c because d is too far away there could not be a continuation.
	      	#for there to be a continuation then d must be in range  if I have ab__ then e must be in range of b - certainly d
	      	#if such a combination were to have a follow-up / continuation

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
		get_colloc(combination,  0, token_map, {nil, nil, <<>>})
	end

	def get_colloc([], _prev_offset, _token_map, acc) do
		acc
	end

	def get_colloc([index | indices], prev_offset, token_map, {first_offset, last_offset, colloc}) do
		#this gets a colloc from a list of sentence offsets - eg [0, 2, 3] - here we have a gap from 0 to 2 (except i think we process colloc backwards [3, 2, 0]
		tok_freq = Map.get(token_map, index)
		_sample_tok_freq = %TokenFreq{freq: 6, index: -1, is_common: false, offset: 3,  token_id: <<0, 0, 0, 30>>} # 'that'	index always seems to be -1 tk
		_sample_tok_frek = %TokenFreq{freq: 2, index: -1, is_common: false, offset: 28, token_id: <<0, 0, 0, 9>>} #  'might'  (offset is from start of sentence - first = 0)

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

		IO.inspect({:new_gap, new_gap}) #don't think we come here

		#shift_new_gap = new_gap * round(:math.pow(2, 6))	 #or left shift 6 times  - use shift_new for very large corpora where we need 4th byte for colloc ids

		<<token_id :: binary-size(1),  rest :: binary-size(3)>> = tok_freq.token_id
		new_token_int = :binary.decode_unsigned(token_id) + new_gap 	#need to update this so that new_gap is two most sig bits of integer - this only works if token_id = 0
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
	def get_pairs(phrase_indices) do
		cutoff = Collocation.get_cutoff()
		#check if we need to reverse the phrase list tk#
		next_indices = case phrase_indices do
	 		[_next_player | opponents] -> opponents
	 		_  -> []
	 	end

		get_pairs(phrase_indices, next_indices, cutoff, [])
	end

	def get_pairs([], [], _cutoff, pair_accum) do
		 #no more battles and no indices left in phrase list.
		 pair_accum
	end

	def get_pairs([], [_h | t] = rest, cutoff, pair_accum) do
		#no battles but there are indices left in phrase list.
		get_pairs(rest, t, cutoff, pair_accum)
	end

	def get_pairs([a | []], phrase_indices, cutoff, pair_accum) do
		#last token in phrase can't start anything - so see if there are more contestants
		next_indices = case phrase_indices do
	 		[next_player | opponents] -> opponents
	 		_  -> []
	 	end
		get_pairs(phrase_indices, next_indices, cutoff, pair_accum)
	end

	def get_pairs([a, b | rest], phrase_indices, cutoff, pair_accum) do
		#winner stays on. a beats b if a is within cutoff of b
		{new_players, new_phrase_indices, new_accum} = if b - a - 1 > cutoff do
		 	#a loses so someone else's turn to play
		 	next_indices = case phrase_indices do
		 		[next_player | opponents] -> opponents
		 		_  -> []
		 	end
			{phrase_indices, next_indices, pair_accum}
		else
			# a wins and stays on to play next in the list
			{[a | rest], phrase_indices, [{a, b} | pair_accum]}
		end

		get_pairs(new_players, new_phrase_indices, cutoff, new_accum)
	end


  def get_quartets([]) do  #doubt we come here
		#quartets are rolling sequences of 4 - Enum.chunk does something similar but does not do the tail where the chunk length tapers to 1
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

	def pair_extractor(fPhraseIterator) do
		fn() ->
			{phrase,  fNextPhraseIterator} = fPhraseIterator.()

			if is_nil(phrase) do
				nilFunc = unchained(nil)
				{nilFunc, nilFunc}
			else
				rev_phrase = Enum.reverse(phrase) #might be able to work backwards through the list without this reverse, but for now easier to read output if forwards
				pairs = get_pairs(rev_phrase)
				qUnchained = unchained(pairs)
				nextExtractor = pair_extractor(fNextPhraseIterator)
				{qUnchained, nextExtractor}
			end
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
