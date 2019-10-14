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

	def create_sent_map_from_wfl(wfl_pid) do
		#we could free the hapax here - but they should have been freed by now.
		#cutoff = get_cutoff()
		#could we parallelise this? to do this we would have to replace the reduce mechanism wiht parallel_job - otherwise should be no problem. probably should fo the frequency filter before parralellising
		#anonymous fn({15, {3, 3}}, %{})
		#sent map tels me how to extend a phrase given sentence and last offset
		WFL.get_freq_stream(wfl_pid)
		#WFL.get_wfl(wfl_pid).types	#ideally this would be a stream tk.  can we reduce from a stream - i would have thought so.

		|> Enum.reduce({%{}, 0}, fn({token_key, %WFL_Type{} = wfl_type}, {sent_map, freq_token_count}) ->
			#IO.inspect(sent_map)
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
		end)
	end

	def pre_pair_up({sent_id,  lhs_cont_map}, root_sent_map, colloc_wfl_pid) do
		{:ok, rhs_cont_map} = Map.fetch(root_sent_map, sent_id)
		pair_up(sent_id, lhs_cont_map, rhs_cont_map, colloc_wfl_pid)
	end

	def pair_up(sent_id, lhs_phrase_map, continuation_map, colloc_wfl_pid) do

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

									WFL.addToken(colloc_wfl_pid, %TokenInput{token: phrase_candidate, instance: %TokenInstance{sentence_id: sent_id, offset: {lhs_first_off, rhs_last_off}}})
									#IO.inspect(phrase_candidate)

								end)
							_ ->
									#don't seem to be handling a failure return type
									#IO.puts("Not handling failure to retrieve value from continuation map.")
									#we do come here quite a lot.  Need to check if this is expected behaviour. tk
									nil
						end
					end
				end)
			end)
	 	end)
	end

	def expand_phrases() do
		#get list of all colloc wfls
		#we are going to go through these in order from smallest to largest, look up smallest and stick on the rhs.
		#having a bit of trouble with this.
		#parent_wfl_pid = WFL.get_parent(last_wfl_pid)
		wfl_chain = X_WFL.get_wfl_chain()

		{wfl_pid, colloc_pid, colloc_chain} = case wfl_chain do
			[root_wfl_pid, root_colloc_pid | rest_chain] -> {root_wfl_pid, root_colloc_pid, rest_chain}
			_ -> {nil, nil, []}
		end
		unless is_nil(colloc_pid) do
			#for each frequent phrase in the root colloc (token pairs) add to concretisation dictionary
			#for the incremental approach to concretisation, we are going to have to be able to look up concretisation by token id
			#start Expansion gen_server
			{:ok, _pid} = Expansion.start_link(wfl_pid, colloc_pid)

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
		cutoff = 2
		{wfl, parent_pid, _} = WFL.get_wfl_state(colloc_pid)
		#grandparent_pid = WFL.get_parent(parent_pid)
		#IO.puts("here we are")
		Enum.each(wfl.types, fn({token_key, %WFL_Type{} = wfl_type})  ->
			if wfl_type.freq >= cutoff do
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
		Enum.each(type_ids, fn({phrase_id, _short_phrase}) ->
			token_info = WFL.get_token_info_from_id(wfl_pid, phrase_id)
			if token_info.freq > 1 do
				#we should be able to find phrase_id in Expansion.phrase_map
				z = Expansion.get_phrase(phrase_id)
				IO.inspect({phrase_id, z})
				expanded_phrase = X_WFL.expand_type_id(wfl_pid, phrase_id, false)
				expanded_phrase2 = X_WFL.expand_type_id(wfl_pid, phrase_id, true)
				IO.inspect({:xp, expanded_phrase2})
				phrase_id2 = Expansion.get_phrase_id(expanded_phrase)
				IO.inspect({:phrase_id, phrase_id2})
			end

		end)
		check_expansions(rest_wfls)
	end

	defp do_concretise_phrases([_wfl_pid | []]) do
		IO.puts("That's all folks")
	end

	defp do_concretise_phrases([colloc_pid | rest_pids]) do
		#cutoff = 2
		#we want to set up a stream here iterating through wfls
		#for the moment only, process a complete list of phrases from the last wfl
		#phrases = WFL.get_wfl(colloc_pid).types
		#frequent_phrases = Enum.filter(phrases, fn({_,  wfl_type}) ->
		#	_sample_colloc = {<<0, 0, 0, 93, 0, 0, 0, 183, 0, 0, 0, 101>>,
		#			 %{concretisations: [], freq: 1, instances: [{19, {0, 2}}],
		#			  is_common: false, type: <<0, 0, 0, 93, 0, 0, 0, 183, 0, 0, 0, 101>>,
		#			  type_id: <<0, 0, 1, 42>>}}

		#	wfl_type.freq >= cutoff
		#end)
		frequent_phrases = WFL.get_freq_stream(colloc_pid)

		case Enum.take(frequent_phrases, 1) do
			[_h | _t] = frequent_phrases ->
				#we have at least one frequent phrase so process it
				IO.inspect("jelly fish")
				Parallel.pjob2(frequent_phrases, {Collocation, :concretise_phrase, [colloc_pid]})
			_ ->
				colloc_pid
		end
		do_concretise_phrases(rest_pids)
	end

	def concretise_phrases() do
		#create a wfl to hold all the phrases that have already been processed.
		wfl_chain =
			X_WFL.get_wfl_chain()
			|> Enum.reverse

		do_concretise_phrases(wfl_chain)

	end

	defp get_concretiser(wfl_pid, concretiser) do
		#if ABC only concretised by ABCD, then AB, AC, BC are concretised by ABCD, not by ABC"
		#concretiser is handed down until abstraction is more frequent than concretiser

		x = WFL.get_token_info_from_id(wfl_pid, concretiser)
		#i think that x is now simply concretiser - how can that be?
		##IO.inspect({:root_info, x.root_info})
		#if we have a root_info, use that, otherwise use this

		%RootInfo{freq: concretiser_freq, conc: %Concretisation{}} = x.root_info

		case concretiser_freq do
			0 ->
				#use this type's info
				%RootInfo{freq: x.freq,  conc: %Concretisation{pid: wfl_pid, token_id: x.type_id}}
			_ ->
				x.root_info
		end
	end

	def concretise_phrase(phrase, concretiser_pid) do
		{_key, type} = phrase
		#IO.inspect({type.type_id})

		#this looks like a blocking call - we are calling back into wflscratch server from a job started there, so neither job can now proceed.
		# the only way forward for the moment is to call the end function directly so that we stay on this thread.
		#later we need to review which modules have which functions
		expanded_phrase = X_WFL.expand_type_id(concretiser_pid, type.type_id, false)	#false leaves the expansion as binary token ids
		conc_space_count = Utils.get_space_count(expanded_phrase)
		#may not be so simple.  I think we are going to have to find a way not to start this exercise from inside X_WFL
		#IO.inspect({:expanded_phrase, expanded_phrase})

		#if ProcessedPhrases.contains(type.type_id) == false do
			abstractions = lose_one_bin(expanded_phrase)
			# IO.inspect(expanded_phrase)
			# IO.inspect(abstractions)
			#parent_wfl_pid = WFL.get_parent(concretiser_pid)
			root_info = get_concretiser(concretiser_pid, type.type_id)


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

		abstraction_space_count = Utils.get_space_count(next_abstraction)

		size_abstraction = byte_size(next_abstraction)
		if size_abstraction > 0 do
			_jj = Expansion.add_concretisation(next_abstraction, concretiser_info, abstraction_space_count, conc_space_count)
			#IO.inspect({:owner, next_abstraction, :conc, concretiser_id})

		else
			Logger.debug("How can next abstraction size be zero?")
			IO.inspect(next_abstraction)
		end
		make_concrete(rest_abstractions, concretiser_info, conc_space_count)
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

	def add_phrase(phrase, phrases) do
		if length(phrase) > 1 do
			[Enum.reverse(phrase) | phrases]
		else
			phrases
		end
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

	def get_pairs([_a | []], phrase_indices, cutoff, pair_accum) do
		#last token in phrase can't start anything - so see if there are more contestants
		next_indices = case phrase_indices do
	 		[_next_player | opponents] -> opponents
	 		_  -> []
	 	end
		get_pairs(phrase_indices, next_indices, cutoff, pair_accum)
	end

	def get_pairs([a, b | rest], phrase_indices, cutoff, pair_accum) do
		#winner stays on. a beats b if a is within cutoff of b
		{new_players, new_phrase_indices, new_accum} = if b - a - 1 > cutoff do
		 	#a loses so someone else's turn to play
		 	next_indices = case phrase_indices do
		 		[_next_player | opponents] -> opponents
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
