defmodule WFL_Item do
	defstruct([:token_type, :token_id, :freq, instances: []])
end

defmodule TokenInput do
	defstruct([:token, :instance])
end

defmodule TokenInstance do
	defstruct([:sentence_id, :offset])
end


defmodule WFL do
	defstruct([depth: 0, token_types: %{}, token_ids: %{}])	#both token_types and token_ids map into the same wfl_item collections

	@name :wfl
	@defaults %WFL{}	#set default text processing module here
	
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
		end		
		{:ok, wfl}
	end

	def handle_call({:add_token, %TokenInput{token: token, instance: %TokenInstance{sentence_id: sentence_id, offset: offset}}}, _from, wfl) do
		#if token is in wfl, update frequency and add sentence/offset/length info 
		#included in token_info should be a pid to higher level wfl which can be used to deconstruct compound types
		#unless raw tokens, such as "cat" are being passed in, in which case pid is nil

		#wfl.token_types is dictionary (map) of occurences of each type - keyed on the plain text of the type, eg "cat"
		#we also need to be able to key on token-id.
		#token id should be stored in wfl_item

		wfl_item = Map.get(wfl.token_types, token)
			case wfl_item do
				%{freq: freq, instances: instances} ->	
					%WFL_Item{token: token, token_id: tokenID, freq: freq + 1, instances: [ %TokenInstance{sentence_id: sentence_id, offset: offset} | instances]}
				_ ->
					#token does not exist in wfl - add token_info to instances collection
					tokenID = TokenCounter.get_token_id()
					%WFL_Item{token: token, token_id: tokenID, freq: 1, instances: [%TokenInstance{sentence_id: sentence_id, offset: offset}]}
			end

		#update wfl maps so we can retrievewfl-items on token or token-id
		new_toke_types_map = Map.update(wfl.token_types, token, wfl_item, wfl_item)
		new_toke_id_map = Map.update(wfl.token_ids, tokenID, wfl_item, wfl_item)	#check that update does an insert if key does not exist.  i think it does.
		new_wfl = Map.update(wfl, token_types: new_toke_types)
			 
		{:reply, :ok, new_wfl}	#don't think we need to return anything.
	end

	def handle_cast( {:push, new}, stack) do
		{:noreply, [new|stack]}
	end
	
	def handle_cast( {:token_id, token}, stack) do
		s = process_wfl(filePath)
		{:noreply, stack}
	end

	defp process_wfl(filePath) do
		#open file - add each token to stack
		File.stream!(filePath)  |> Enum.each(fn (line) ->
  			#:gen_server.cast(:WFL, {:push, line})
  			tokenise(String.codepoints(line))  			
		end)			
	end

	defp tokenise([]) do

	end

	defp tokenise([h|t]) do
		#check if h is a stop char - for this we might want to consult an agent
		#although if we have a distributed system all nodes should have their own copy
		
		#res = Agent.get(@stoppers, fn hd -> HashDict.get(hd, h) end)
		unless nil do
			#we have a stopper
			:gen_server.cast(:WFL, {:push, h})
		end
		
		tokenise(t)
	end
end
