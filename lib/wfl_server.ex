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

	def get_wfl_stream(key) do
		:gen_server.call(:WFL, {:get_wfl_stream, key})
	end

	def get_wfl_pid(key) do
		:gen_server.call(:WFL, {:get_wfl_pid, key})		
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
		
		{:noreply, new_state}	#note that new_state will not yet have an up-to-date wfl - we get a handle_info notification when file has been read.
	end
	
	def handle_call({:get_wfl_raw, key}, _client, state) do
		wfl_pid = Map.get(state, key)
		wfl = WFL.get_wfl(wfl_pid)
		{:reply, wfl, state}
	end

	def handle_call({:get_wfl_pid, key}, _client, state) do
		wfl_pid = Map.get(state, key)		
		{:reply, wfl_pid, state}
	end

	def handle_call({:get_token_info, key, token}, _client, state) when is_pid(key) or is_atom(key) do		
		wfl_item = WFL.get_token_info(key, token)
		{:reply, wfl_item, state}
	end

	def handle_call({:get_token_info, key, token}, _client, state) do
		wfl_pid = Map.get(state, key)		
		wfl_item = WFL.get_token_info(wfl_pid, token)
		{:reply, wfl_item, state}
	end
	
	def handle_call({:get_wfl, key, field, order}, _client, state) when is_pid(key) or is_atom(key) do
		sorted_wfl = get_sorted_wfl(key, field,  order)
			
		{:reply, sorted_wfl, state}
	end
	
	def handle_call({:get_wfl, key, field, order}, _client, state) do
		wfl_pid = Map.get(state, key)
		sorted_wfl = get_sorted_wfl(wfl_pid, field,  order)
			
		{:reply, sorted_wfl, state}
	end
	def handle_info( {:file_complete, wfl_pid}, state) do
		IO.puts "Handle info: File read: complete - next make a call into wfl to see what it has got."
		#mark grammar/common words - for the moment just using ["the", "a", "an"] - we should add these at the start.
		WFL.mark_common(wfl_pid, ["the", "a"])
		process_collocations(wfl_pid)				
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


	def get_sorted_wfl(wfl_pid, field, order) do #this should be on wfl
		#IO.inspect(wfl_pid)
		wfl_types = WFL.get_wfl(wfl_pid).types
		wfl = Map.to_list(wfl_types)
		
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
	end

	defp process_collocations(source_wfl_pid) do
		cutoff = 2	#this will have to be in config or similar.
		
		# get the list of wfl_items 		
		sorted_wfl = get_sorted_wfl(source_wfl_pid, :freq, :desc)
      	filtered_list = Enum.take_while(sorted_wfl, fn({_key, item}) -> item.freq >= cutoff end)

		#for the moment use the parent_wfl_pid to store source.  this may not end up as method of choice
		{:ok, colloc_wfl_pid} = WFL.start_link(source_wfl_pid)
		#this iterates each binary representation of sentence - but not by going to sentence list as it probably should - it goes straight to the store
		#we need to get passed in a list of sentences and iterate that - or have a different tokens_binary for phrases.
		tb_s = Stream.map(TokensBinary.get_map(), fn(tok_bin) -> tok_bin end)
		Parallel.pjob(tb_s, [{Collocation, :say_hello, [colloc_wfl_pid]}])
		process_collocs(colloc_wfl_pid)
	end

	def process_collocs(source_wfl_pid) do
		cutoff = 1	#get from config
		p_s_t = WFL.get_wfl(source_wfl_pid).types		
		p_s = Enum.filter(p_s_t, fn({_,  wfl_type}) -> 
			_sample_p_s = {<<0, 0, 0, 93, 0, 0, 0, 183, 0, 0, 0, 101>>,
					 %{concretisations: [], freq: 1, instances: [{19, {0, 2}}],
					  is_common: false, type: <<0, 0, 0, 93, 0, 0, 0, 183, 0, 0, 0, 101>>,
					  type_id: <<0, 0, 1, 42>>}}


			#IO.inspect({11, wfl_type}) 
			wfl_type >= cutoff 
		end)	#note - this part iterates so we need to call a holding function.

		case p_s do
			[_h | _t] = p_s ->
				#we have at least one frequent colloc so process it
				{:ok, colloc_wfl_pid} = WFL.start_link(source_wfl_pid)
		  
				Parallel.pjob(p_s, [{Collocation, :do_phrase, [colloc_wfl_pid]}])
				IO.inspect("whoheee")
				process_collocs(colloc_wfl_pid)
			_ ->
				nil	
		end
	end

	defp merge_wfls(_a, accum) do
		accum
	end
	

	
end
