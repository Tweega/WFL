#wfl_types.ex
#end wfl_types.ex

defmodule WFL do	
	@defaults %WFL_Data{}	#set default text processing module here
	
	use GenServer
	
	#API	
	def addToken(pid, %TokenInput{} = token_info) do
		:gen_server.call(pid, {:add_token, token_info })
	end

	def start_link() do		#pass in an initial wfl?
		:gen_server.start_link(__MODULE__, %WFL_Data{}, [])		
	end

	#Server
	def init(initial_wfl) do		
		#the state of a wfl is a WFL
			
		{:ok, initial_wfl}
	end

	def handle_call({:add_token, %TokenInput{token: token, instance: %TokenInstance{sentence_id: sentence_id, offset: offset}}}, _from, wfl) do
		#if token is in wfl, update frequency and add sentence/offset/length info 
		#included in token_info should be a pid to higher level wfl which can be used to deconstruct compound types
		#unless raw tokens, such as "cat" are being passed in, in which case pid is nil

		#wfl.types is dictionary (map) of occurences of each type - keyed on the plain text of the type, eg "cat"
		#we also need to be able to key on token-id.
		#token id should be stored in WFL_Type

		wfl_type = Map.get(wfl.types, token)

		new_wfl_type =
			case wfl_type do
				%WFL_Type{freq: freq, instances: instances} ->	
					%WFL_Type{wfl_type | freq: freq + 1,  instances: [%TokenInstance{sentence_id: sentence_id, offset: offset} | instances]}					
				_ ->
					#token does not exist in wfl - add token_info to instances collection
					type_id = TokenCounter.get_type_id()
					%WFL_Type{type: token, type_id: type_id, freq: 1, instances: [%TokenInstance{sentence_id: sentence_id, offset: offset}]}
			end

		#update wfl maps so we can retrieve wfl-items on token or token-id
		new_toke_types = Map.update(wfl.types, token, new_wfl_type, new_wfl_type)
		new_toke_ids = Map.update(wfl.type_ids, new_wfl_type.type_id, new_wfl_type, new_wfl_type)	#check that update does an insert if key does not exist.  i think it does.
		new_wfl = %WFL_Data{wfl | types: new_toke_types, type_ids: new_toke_ids}

		Map.update(wfl, types: new_toke_types)
			 
		{:reply, :ok, new_wfl}	#don't think we need to return anything.
	end

	def handle_cast({:add_tokens, sentences}, %WFL_Data{} = wfl_data) do	
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
			Sentences.new(sentence_id, sentence)

			{_token_offset, tokens_binary, wfl_data2} = process_tokens(tokens, sentence_id, wfl_data1)

			#store tokens binary data - this will be the input 'text' for the next round of tokens.
			TokensBinary.new(sentence_id, tokens_binary)
			wfl_data2			
		end)
	#state contains wfl_data
		{:noreply, new_wfl_data}
	end

	defp process_tokens(tokens, sentence_id, %WFL_Data{} = wfl_data) do
		
		#we want to end up with 
		#		defstruct([:type, :type_id, :freq, instances: []]) WFL_Type for each new token and with instances/freq update for existing types
		# we also want sentences of the form <<tok_id1 <> tok_id2>>...a binary string - so we are not storing the actual text here - should this be binary string or 

		#we want a map of tokens so that we have [97, 123, 554, 98, 222, 27] etc. for e.g. the cat sat on the mat....except we want that as a binary string with 4 bytes per number


		#update / create WFL_Type for each token
		List.foldl(tokens, {0, <<"">>, wfl_data}, fn (token, {token_offset, tokens_binary, wfl_data1}) -> 
			{token_id, wfl_data2} = process_token(token, sentence_id, token_offset, wfl_data1)				
			{token_offset + 1, << token_id <> tokens_binary >>,  wfl_data2} end			
		)		
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


	def to_4_bytes(l) when is_integer(l) do
	 <<l :: integer-unit(8)-size(4)>>
	end

	def to_4_bytes(l) do
		Enum.reduce(Enum.reverse(l), <<>>, fn(t, acc) -> << <<t :: integer-unit(8)-size(4)>> <> acc >> end)
	end

end
