#wfl_types.ex
defmodule OffsetMaps do
  defstruct([token_map: %{}, combination_map: %{}])
end
#end wfl_types.ex

defmodule WFL do	
	@defaults %WFL_Data{}	#set default text processing module here
	
	use GenServer
	
	#API	
	def getFrequentTypes(pid, cutoff) do
		:gen_server.call(pid, {:frequent_types, cutoff })
	end

	def addToken(pid, %TokenInput{} = token_info) do
		:gen_server.call(pid, {:add_token, token_info })
	end

	def addColloc(pid, %TokenInput{} = token_info) do  	#defstruct([:token, :instance])		may not use this api 
		:gen_server.call(pid, {:add_colloc, token_info })
	end

	def get_wfl(wfl_pid) do		
		:gen_server.call(wfl_pid, :get_wfl)
	end

	def get_wfl_state(wfl_pid) do		
		:gen_server.call(wfl_pid, :get_state)
	end

	def get_token_info(wfl_pid, token) do
		:gen_server.call(wfl_pid, {:get_token_info, token})
	end

	def get_token_info_from_id(wfl_pid, token_id) do 	#can't distinguish between binary and string so have to make a different api call
		:gen_server.call(wfl_pid, {:get_token_info_from_id, token_id})
	end

	def get_token_from_id(wfl_pid, token_id) do 	#this returns {token, state} unlike get_token_info - may need to rename this.
		:gen_server.call(wfl_pid, {:get_token_from_id, token_id})
	end
	

	def get_parent(wfl_pid) do		
		:gen_server.call(wfl_pid, :get_parent)
	end

	def expand_type_id(wfl_pid, type_id, include_root \\ true) do		
		:gen_server.call(wfl_pid, {:expand_type_id, type_id, include_root})
	end

	def mark_common(wfl_pid, type_list) do		
		:gen_server.call(wfl_pid, {:mark_common, type_list})
	end

	def start_link(parent_wfl_pid \\ nil) do		#pass in an initial wfl?
		:gen_server.start_link(__MODULE__, {%WFL_Data{}, parent_wfl_pid}, [])		
	end

	#Server
	def init(state) do		
		#the state of a wfl is a WFL
			
		{:ok, state}
	end

	def handle_call(:get_state, _from, state) do
		{:reply, state, state}
	end

	def handle_call(:get_wfl, _from, {wfl, _parent} = state) do
		{:reply, wfl, state}
	end

	def handle_call(:get_parent, _from, {_wfl, parent} = state) do
		{:reply, parent, state}
	end

	def handle_call({:expand_type_id, type_id, include_root}, _from, state) do
		wfl_pid = self()
		x = expand_token(type_id, [wfl_pid], <<>>, include_root)
		{:reply, x, state}
	end

	def handle_call({:add_token, %TokenInput{token: token, instance: %TokenInstance{sentence_id: sentence_id, offset: offset}}}, _from, {%WFL_Data{} = wfl_data, parent_wfl_pid}) do
		
		{token_id, new_wfl} = process_token(token, sentence_id, offset, wfl_data)				

		{:reply, {:ok, token_id}, {new_wfl, parent_wfl_pid}}	
	end

	def handle_call({:get_token_info, token}, _client, {%WFL_Data{} = wfl_data, _parent_wfl_pid} = state) do
		wfl_item = fetch_token_info(wfl_data, token)
		{:reply, wfl_item, state}
	end

	def handle_call({:get_token_info_from_id, token_id}, _client, {%WFL_Data{} = wfl_data, _parent_wfl_pid} = state) do
		wfl_item = fetch_token_info_from_id(wfl_data, token_id)
		{:reply, wfl_item, state}
	end

	def handle_call({:get_token_from_id, token_id}, _client, state) do
		token_parent = fetch_token_from_id(state, token_id)
		{:reply, token_parent, state}
	end

	def handle_call({:mark_common, common_list}, _from, {wfl, parent}) do

		new_wfl_types = Enum.reduce(common_list, wfl.types, fn(common_token, wfl_types) -> 			
			Map.update(wfl_types, common_token, %WFL_Type{}, fn(wfl_info) -> 
				%WFL_Type{wfl_info | is_common: true} 
			end)				
		end)

		{:reply, :ok, {%WFL_Data{wfl | types: new_wfl_types}, parent}}
	end


	def handle_call(:frequent_types, _from, {wfl, _parent} = state) do
		{:reply, wfl, state}
	end

	#{<<"first_off">>, <<"last_off">>, <<1, 0, 0, 130, 2, 0, 0, 120, 0, 0, 0, 109>>}

	
	def handle_cast({:add_tokens, sentences}, {%WFL_Data{} = wfl_data, parent_wfl_pid}) do	
		#at the end of this process
			#ok - all sentences in sentences should be saved somewhere keyed on sentence_id
			#ok  jut to be added to map - all tokens in a sentence should be merged into a binary format containing the token ids only, 4 bytes per token
			#ok the wfl type map should be updated so that there is wfl_info for each token with updated instances collection and frequency
			#ok the wfl_id map should be updated where there is a new token_type.

		#for each sentence get a sentence id and for each token get token id, and reconstruct sentence from that
		
		new_wfl_data = List.foldl(sentences, wfl_data, fn (%SentenceInfo{tokens: tokens, sentence: sentence}, wfl_data1) -> 
			#get a sentence id
			sentence_id = SentenceCounter.get_sentence_id()

			#save the sentence somewhere - we don't need it for a while.  We could even store only offset and length in a filename bucket
			#this could/should be in a different process

			
			Sentences.new(sentence_id, sentence)
			
			{_token_offset, tokens_binary, offset_map, wfl_data2} = process_tokens(tokens, sentence_id, wfl_data1)
			
			_sample_offset_map = %{0 => <<0, 0, 0, 4>>, 1 => <<0, 0, 0, 188>>, 2 => <<0, 0, 0, 158>>}
			#IO.inspect(offset_map)

			#store tokens binary data - this will be the input 'text' for the next round of tokens.			
			TokensBinary.new(sentence_id, %TokensBinary{bin_tokens: tokens_binary, offset_maps: %OffsetMaps{token_map: offset_map}})
			wfl_data2			
		end)
		
		{:noreply, {new_wfl_data, parent_wfl_pid}}
	end


	def deep_lookup(lookup) do
		deep_lookup(lookup, [])
	end

	def deep_lookup([], acc) do
		#stop search when (include_root is true AND parent_wfl is null) OR grandparent_wfl is null - any other way to tell this?
		acc
	end

	def deep_lookup([h | t], acc) do
		
		case h do
			1 -> 			
				IO.inspect(t)
		
				deep_lookup(t, [h | acc])
			_ -> 			
				deep_lookup([h - 1 | [1 | t]], acc)			
		end
	end

	defp deep_lookup({%WFL_Data{} = wfl_data, parent_wfl_pid} = state, wfl_pid, token_id) do
		#this is here only by way of a backup in case we implement deep_search at some point.
		#fetch_token_info_from_id - deep version of fetch_token_info_from_id - searches parent wfl if not found locally
		
		search_result = Map.fetch(wfl_data.type_ids, token_id)
		
		{where_to_continue, where_found, search_token} = case search_result do
			{:ok, res} ->
				{nil, wfl_pid, res}	#why do we need the pid of wfl where found? -so that we can continue to expand token.

			{:error, _res} ->
				#this wfl doesn't have the token - try parent wfl
				parent_state = WFL.get_wfl_state(parent_wfl_pid)				
				{parent_state, parent_wfl_pid, token_id}
		end

		deep_lookup(where_to_continue, where_found, search_token)
	end

	defp deep_lookup(_, _, nil) do
		#ditto above
		IO.puts("error: nil search term token id")
	end

	defp deep_lookup(nil, where_found, search_token) do
		#ditto above
		{nil, where_found, search_token}	#where_found is a wfl pid - has this been found, though?
	end


	defp expand_phrase(key, wfl_data, include_root, parent_wfl_pid) do

		#key may be <<0,0,0,3,  0,0,0,4>> in which case we have two lookups - note that we have to drop the left most 4byte as that indicates a space count
		#how will the space count be handled?.  Note that the space count is part of the token_id so leave on to retrieve -just a presentation thing.
		#need to find a way to manage the place holders cat * on the mat
		phrase = expand_phrase(key)
	end

	defp process_tokens(tokens, sentence_id, %WFL_Data{} = wfl_data) do
		
		#we want to end up with 
		#		defstruct([:type, :type_id, :freq, instances: []]) WFL_Type for each new token and with instances/freq update for existing types
		# we also want sentences of the form <<tok_id1 <> tok_id2>>...a binary string - so we are not storing the actual text here - should this be binary string or 

		#we want a map of tokens so that we have [97, 123, 554, 98, 222, 27] etc. for e.g. the cat sat on the mat....except we want that as a binary string with 4 bytes per number
#IO.inspect(tokens)

		#update / create WFL_Type for each token	
		toke_offset = length(tokens) -1 # remove -1 for 1 based vs 0 based index.
		List.foldl(tokens, {toke_offset, <<"">>, %{}, wfl_data}, fn (token, {token_offset, tokens_binary, token_offset_map, wfl_data1}) -> 
			{token_id, wfl_data2} = process_token(token, sentence_id, token_offset, wfl_data1)				
			offset_map = Map.put(token_offset_map, token_offset, token_id)
			{token_offset - 1, << token_id <> tokens_binary >>, offset_map, wfl_data2}	#next accumulator value		
		end)		
	end

	defp process_token(token, sentence_id, offset, %WFL_Data{types: types, type_ids: type_ids}) do

		offsets = case offset do
			{first_off, last_off} ->
				{first_off, last_off}
				
			first_off ->
				{first_off, first_off}
		end
		
		new_instance = {sentence_id,  offsets}

		#see if we already have this token in wfl
		{type_id, new_types} = Map.get_and_update(types, token, fn type_info -> 			
			new_type_info = case type_info do				
				%WFL_Type{freq: freq, instances: instances} ->					
					#existing type
					%WFL_Type{type_info | freq: freq + 1, instances: [new_instance | instances]}			
				_ ->
					#token not seen before
					new_type_id = TokenCounter.get_token_id()				
					%WFL_Type{type: token, type_id: new_type_id, freq: 1, instances: [new_instance]}
			end
			{new_type_info.type_id, new_type_info}
		end)


		#we want to be able to look up type from id
		new_type_ids = Map.put_new(type_ids, type_id, token)

		{type_id, %WFL_Data{types: new_types, type_ids: new_type_ids}}
	end

	def expand_token(<<>>, [wfl_pid | _rest], phrase, to_text) when to_text == 1 do 
		#terminal case where all token_ids have been 'translated' BUT we still have to translate token_ids into text equivalents
		expand_token_text(wfl_pid, phrase)
		
	end

	def expand_token(<<>>, [_wfl_pid | _rest], phrase, _to_text) do 
		#terminal case where all token_ids have been 'translated'
		Utils.rev_bin(phrase)	
	end

	def expand_token(<<token_id :: binary-size(4), rest :: binary>>, [wfl_pid | pids], phrase, to_text) when to_text == 1 do 
		#main expand function
		
		{result, parent_wfl_pid} = WFL.get_token_from_id(wfl_pid, token_id)
		
		{new_phrase, new_token_stack, new_wfl_stack} =
			if is_nil(parent_wfl_pid) do
				#this is a terminating condition - token_id found at root colloc level.
				{result <> phrase, rest, pids}
			else
				result_len = round(byte_size(result) / 4)
				new_pids = List.duplicate(wfl_pid, result_len)
				token_stack = result <> rest
				wfl_stack = new_pids ++ pids
				{phrase, token_stack, wfl_stack}
			end
		expand_token(new_token_stack, new_wfl_stack, new_phrase, to_text)
		
	end

	def expand_token_text(wfl_pid, tokens) do
		#for each token_id in phrase - look up the token
		wfl_type_ids = WFL.get_wfl(wfl_pid).type_ids
		expand_token_text(wfl_type_ids, tokens, [])

	end

	def expand_token_text(_wfl_type_ids, <<>>, acc) do
		acc
	end

	def expand_token_text(wfl_type_ids, <<token_id :: binary-size(4), rest :: binary>>, acc) do
		#for each token_id in phrase - look up the token id
		token = Map.get(wfl_type_ids, token_id)
		expand_token_text(wfl_type_ids, rest, [token | acc])
	end
	
	defp fetch_token_from_id({%WFL_Data{} = wfl_data, parent_wfl_pid}, token_id) do
		#returns the type denoted by a token id eg 123 -> cat, 234 -> sat, 222 -> [123, 234] or "cat sat".
		
		# we used to try searching up the tree in the event of not finding an item.
		#may need to end up doing this - but for now assuming that only need to search one level

		res = Map.get(wfl_data.type_ids, token_id)
		#do i always join things from the same wfl? - i think so - then we don't need deep search..
		{res, parent_wfl_pid}
	end


	defp fetch_token_info_from_id(wfl, token_id) do
		token = Map.get(wfl.type_ids, token_id)
		Map.get(wfl.types, token)
	end

	defp fetch_token_info(wfl, token) do
		Map.get(wfl.types, token)
	end


	def to_4_bytes(l) when is_integer(l) do
	 <<l :: integer-unit(8)-size(4)>>
	end

	def to_4_bytes(_l) do
		
	end

end
