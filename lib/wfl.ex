#wfl_types.ex
#end wfl_types.ex
defmodule CollocInfo do
	defstruct([:bin_tokens, :sent_id, :offset])
end

defmodule WFL do	
	@defaults %WFL_Data{}	#set default text processing module here
	
	use GenServer
	
	#API	
	def addToken(pid, %TokenInput{} = token_info) do
		:gen_server.call(pid, {:add_token, token_info })
	end

	def get_wfl(wfl_pid) do		
		:gen_server.call(wfl_pid, :get_wfl)
	end

	def get_token_info(wfl_pid, token) do
		:gen_server.call(wfl_pid, {:get_token_info, token})
	end

	def get_parent(wfl_pid) do		
		:gen_server.call(wfl_pid, :get_parent)
	end

	def expand_type_id(wfl_pid, type_id) do		
		:gen_server.call(wfl_pid, {:expand_type_id, type_id})
	end

	def start_link(parent_wfl_pid \\ nil) do		#pass in an initial wfl?
		:gen_server.start_link(__MODULE__, {%WFL_Data{}, parent_wfl_pid}, [])		
	end

	#Server
	def init(state) do		
		#the state of a wfl is a WFL
			
		{:ok, state}
	end

	def handle_call(:get_wfl, _from, {wfl, _parent} = state) do
		{:reply, wfl, state}
	end

	def handle_call(:get_parent, _from, {_wfl, parent} = state) do
		{:reply, parent, state}
	end

	def handle_call({:expand_type, type_id}, _from, state) do
		parent = 12333
		{:reply, parent, state}
	end

	
	def handle_call({:add_token, %TokenInput{token: token, instance: %TokenInstance{sentence_id: sentence_id, offset: offset}}}, _from, {%WFL_Data{} = wfl_data, parent_wfl_pid} = state) do
		
		{token_id, new_wfl} = process_token(token, sentence_id, offset, wfl_data)				

		{:reply, {:ok, token_id}, {new_wfl, parent_wfl_pid}}	
	end

	def handle_call({:get_token_info, token}, _client, state) do
		wfl_item = fetch_token_info(state, token)
		{:reply, wfl_item, state}
	end

	
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
			
			{_token_offset, tokens_binary, wfl_data2} = process_tokens(tokens, sentence_id, wfl_data1)

			#store tokens binary data - this will be the input 'text' for the next round of tokens.			
			TokensBinary.new(sentence_id, %TokensBinary{bin_tokens: tokens_binary})
			wfl_data2			
		end)
		
		{:noreply, {new_wfl_data, parent_wfl_pid}}
	end


	def handle_cast({:add_collocations, collocs}, {%WFL_Data{} = wfl_data, parent_wfl_pid}) do	
		#at the end of this process
			#what we have is a collection of types in binary form, the sentence they are from and the offset to first token
			#[{bin_tokens, sent_id, offset}...]  as in [{<<82, 12>>, 21, 2}...]

			new_wfl_data = List.foldl(collocs, wfl_data, fn ({bin_tokens, sentence_id, token_offset}, wfl_data1) -> 
			
			#we want to add new type to the wfl along with instance data
			{_token_id, wfl_data2} = process_token(bin_tokens, sentence_id, token_offset, wfl_data1)

			wfl_data2
		end)
		
		{:noreply, {new_wfl_data, parent_wfl_pid}}
	end

defp expand_type({wfl, parent} , key) do

		#we may have token id <<0,0,0,3,  0,0,0,4>> in which case we have two lookups - we may also have a  space in the middle WORKING HERE
		token = fetch_token_info(wfl, key)
		x = case token do
			%WFL_Type{} ->
				case parent do
					nil -> token
					_ -> WFL.expand_type(parent, token)
				end

			_ -> nil
		end

	end
	

	defp process_tokens(tokens, sentence_id, %WFL_Data{} = wfl_data) do
		
		#we want to end up with 
		#		defstruct([:type, :type_id, :freq, instances: []]) WFL_Type for each new token and with instances/freq update for existing types
		# we also want sentences of the form <<tok_id1 <> tok_id2>>...a binary string - so we are not storing the actual text here - should this be binary string or 

		#we want a map of tokens so that we have [97, 123, 554, 98, 222, 27] etc. for e.g. the cat sat on the mat....except we want that as a binary string with 4 bytes per number
#IO.inspect(tokens)

		#update / create WFL_Type for each token	
		toke_offset = length(tokens)	# -1 if we want zero based index.  as it is the first offset is 1
		List.foldl(tokens, {toke_offset, <<"">>, wfl_data}, fn (token, {token_offset, tokens_binary, wfl_data1}) -> 
			{token_id, wfl_data2} = process_token(token, sentence_id, token_offset, wfl_data1)				
			{token_offset - 1, << token_id <> tokens_binary >>,  wfl_data2}	#next accumulator value		
		end)		
	end

	defp process_token(token, sentence_id, offset, %WFL_Data{types: types, type_ids: type_ids}) do
		new_instance = {sentence_id,  offset}

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

	defp fetch_token_info(wfl, token_id) when is_integer(token_id) do		
		token = Map.get(wfl.type_ids, token_id)		
		Map.get(wfl.wfl_types, token)
	end

	defp fetch_token_info(wfl, token) do
		Map.get(wfl.wfl_types, token)
	end


	def to_4_bytes(l) when is_integer(l) do
	 <<l :: integer-unit(8)-size(4)>>
	end

	def to_4_bytes(_l) do
		
	end

end
