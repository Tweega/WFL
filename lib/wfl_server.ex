defmodule WFLScratch.Server do
	use GenServer
	@name :WFL
	
	#API
	#we only have one of these? this creates a text reader for each file (could be handed an array of files or a wildcard file path)
	#and then collates all the results

	def processFile(filePath, readerModule) do
		:gen_server.cast(:WFL, {:wfl_file, {filePath, readerModule}})
	end

	def get_wfl_raw(key) do	
		#key identifies an alias for a given wfl.
		:gen_server.call(:WFL, {:get_wfl_raw, key})
	end

	def get_wfl(key, field \\ :freq, order \\ :desc) do		
		:gen_server.call(:WFL, {:get_wfl, key, field, order})
	end

	def get_token_info(key, token) do
		:gen_server.call(:WFL, {:get_token_info, key, token})
	end

	def start_link(_x) do 	#we could initialise with an existing wfl or lemma file? if so we could spawn the process that reads those in.
		:gen_server.start_link({:local, @name}, __MODULE__, %{}, [])
	end

	#Server
	def init(%{} = state) do
		#stack is map with empty list of readers as its only element.
		{:ok, state}
	end
	
	def handle_cast( {:wfl_file, {filePath, readerModule}}, state) do
		{:ok, wfl_pid} = WFL.start_link()	#store this in map with filename as key? we need to have a completed status flag in state
		new_state = Map.put_new(state, filePath, wfl_pid)	#check if a wfl for this filename already exists
		process_file(filePath, readerModule, wfl_pid)	#should process_file be async?  what is the effect of handle_info calls which set state before this call terminates?
		{:noreply, new_state}
	end
	
	def handle_call({:get_wfl_raw, key}, _client, state) do
		wfl_pid = Map.get(state, key)
		wfl = WFL.get_wfl(wfl_pid)
		{:reply, wfl, state}
	end

	def handle_call({:get_token_info, key, token}, _client, state) do
		wfl_pid = Map.get(state, key)
		wfl = WFL.get_wfl(wfl_pid).types
		wfl_item = Map.get(wfl, token)
		{:reply, wfl_item, state}
	end

	def handle_call({:get_wfl, key, field, order}, _client, state) do
		wfl_pid = Map.get(state, key)
		wfl_types = WFL.get_wfl(wfl_pid).types
		wfl = Map.to_list(wfl_types)
		[{_a, h} | _t] = wfl
		IO.inspect(h.freq)
		sorted_wfl = 

case field do    #a nuisance here that structs don't implement access behaviour
	:freq ->
		case order do
			:asc ->
				Enum.sort(wfl, fn ({_, wfl_item_a}, {_, wfl_item_b}) -> wfl_item_a.freq <= wfl_item_b.freq end)
			_ -> 
				Enum.sort(wfl, fn ({_, wfl_item_a}, {_, wfl_item_b}) -> wfl_item_a.freq >= wfl_item_b.freq end)
		end
	_ ->
		case order do
			:asc ->
				Enum.sort(wfl, fn ({_, wfl_item_a}, {_, wfl_item_b}) -> wfl_item_a.type <= wfl_item_b.type end)
			_ -> 
				Enum.sort(wfl, fn ({_, wfl_item_a}, {_, wfl_item_b}) -> wfl_item_a.type >= wfl_item_b.type end)
		end
	
end	
		{:reply, sorted_wfl, state}
	end
	
	def handle_info( {:file_complete, _filePath}, state) do
		IO.puts "Handle info: File read: complete - next make a call into wfl to see what it has got."
		{:noreply, state}
	end

	def handle_info( {:file_error, _filePath}, state) do
		IO.puts "Handle info File error: Something else"
		{:noreply, state}
	end

	defp process_file(filePath, readerModule, wfl_pid) do
		#here we create a text-reader process and get it to report back every so often with tokens/sentences
		
		char_def_tree = CharClass.new()
		me = self()
		
		apply(readerModule, :processText, [filePath, char_def_tree, me, wfl_pid])	#handle_info will be called when finished
		
		#readers = [reader | readers]
		
	end
	
end
