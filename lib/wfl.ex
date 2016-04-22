defmodule WFL_Item do
	defstruct([:token_type, :freq, instances: []])
end

defmodule TokenInfo do
	defstruct(:token, :instance)
end

defmodule TokenInstance do
	defstruct(:sentence_id, :offset)
end


defmodule WFL do
	defstruct([depth: 0, token_types: %{}, token_ids: %{}])	#WFL has a map keyed on token-types eg "cat" - we also need a reverse lookup

	@name :wfldata
	
	use GenServer
	

	#API	
	def addToken(%TokenInfo{} = token_info) do

		:gen_server.call(@name, {:add_token, token_info })
	end

	def start_link(depth) do
		:gen_server.start_link({:local, :WFL}, __MODULE__, %{:depth => depth, :wfl_list => []}, [])		
	end

	#Server
	def init(_no_initial_state) do		
		#the main state of a wfl is a WFL
		wfl = %WFL{}
		{:ok, wfl}
	end

	def handle_call({:add_token, %TokenInfo{token: token, sentence_id: sentence_id, offset: offset}}, _from, wfl) do
		#if token is in wfl, update frequency and add sentence/offset/length info 
		#included in token_info should be a pid to higher level wfl which can be used to deconstruct compound types
		#unless raw tokens, such as "cat" are being passed in, in which case pid is nil

			wfl_item = Map.get(wfl.token_types, token)
				case wfl_item do
					%{freq: freq, instances: instances} ->	
						%WFL_Item{token: token, freq: freq + 1, instances: [ %TokenInstance{sentence_id: sentence_id, offset: offset} | instances]}
					_ ->
						#token does not exist in wfl - add token_info to instances collection
						%WFL_Item{token: token, freq: 1, instances: [%TokenInstance{sentence_id: sentence_id, offset: offset}]}
				end
%{expenses | groceries: 150, commute: 75}

new_toke_types = Map.update(wfl.token_types, token, wfl_item, wfl_item)
new_wfl = Map.update(wfl, token_types: new_toke_types)

				
			 
		{:reply, wfl_item, new_wfl}
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
