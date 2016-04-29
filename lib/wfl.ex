defmodule WFL_Type do
	defstruct([:type, :type_id, :freq, instances: []])
end

defmodule TokenInput do
	defstruct([:token, :instance])
end

defmodule TokenInstance do
	defstruct([:sentence_id, :offset])
end

defmodule WFL_Data do
	defstruct([depth: 0, types: %{}, type_ids: %{}])	#both types and type_ids map into the same WFL_Type collections
end

defmodule WFL do
	
	@name :wfl
	@defaults %WFL_Data{}	#set default text processing module here
	
	use GenServer
	

	#API	
	def addToken(%TokenInput{} = token_info) do

		:gen_server.call(@name, {:add_token, token_info })
	end

	def start_link(initial_wfl) do		
		:gen_server.start_link(__MODULE__, initial_wfl, [])		
	end

	#Server
	def init(initial_wfl) do		
		#the state of a wfl is a WFL
		wfl = 
		case initial_wfl do
			%{} -> Map.merge(@defaults, initial_wfl)
			_   -> @defaults			
		end		
		{:ok, wfl}
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

	def handle_cast( {:push, new}, stack) do
		{:noreply, [new|stack]}
	end
	
	def handle_cast( {:type_id, _token}, stack) do
		#_s = process_wfl(filePath)
		{:noreply, stack}
	end

end
