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

	def get_colloc_pid() do
		:gen_server.call(:WFL, :get_colloc_pid)		
	end

	def expand_type_id(wfl_pid, type_id, to_text \\ true) do		
		:gen_server.call(:WFL, {:expand_type_id, wfl_pid, type_id, to_text})
	end

	def expand_wfl(wfl_pid, to_text \\ true) do		
		:gen_server.call(:WFL, {:expand_wfl, wfl_pid, to_text})
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
		new_state = Map.put_new(state, "root_wfl_pid", wfl_pid)	#originally used filename as key but want access to root - need another level to manage mutiple files
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

	def handle_call({:expand_type_id, wfl_pid, token_id, to_text}, _from, state) do
		parent_wfl_pid = WFL.get_parent(wfl_pid)
		continuation_wfl_pid = Map.get(state, "root_wfl_pid")

		x = expand_token({token_id, wfl_pid, parent_wfl_pid}, continuation_wfl_pid, [], <<>>, to_text)
		{:reply, x, state}
	end

	def handle_call({:expand_wfl, wfl_pid, to_text}, _from, state) do
		
		root_wfl_pid = Map.get(state, "root_wfl_pid")
		#add a check to see if wfl_pid is the root wfl pid, in which case we just want a dump of the wfl - or at least the types.
		{wfl, parent_pid} = WFL.get_wfl_state(wfl_pid)
		#grandparent_pid = WFL.get_parent(parent_pid)
		Enum.each(wfl.type_ids, fn({token_id, info})  ->
			tok = exp_token(token_id, wfl_pid, parent_pid, root_wfl_pid, to_text)
			#tok = exp_token(key, parent_pid, grandparent_pid}, continuation_wfl_pid, [], <<>>, to_text)
			IO.inspect({:tik, info.freq, tok})
		end)
		
		{:reply, :ok, state}
	end


	def handle_info( {:file_complete, wfl_pid}, state) do
		IO.puts "Handle info: File read: complete - next make a call into wfl to see what it has got."
		#mark grammar/common words - for the moment just using ["the", "a", "an"] - we should add these at the start.
		#if working with multiple files, create common tokens once and clone
		WFL.mark_common(wfl_pid, ["the", "a"])
		last_wfl_pid = process_collocations(wfl_pid)	#capturing last_wfl_pid only needed to allow us to keep it in scope after text has been processed so we ca interrogate from the command line 		
		new_state2 = Map.put_new(state, "last_wfl_pid", last_wfl_pid)
		{:noreply, new_state2}
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
		{root_sent_map, freq_token_count} = Collocation.create_sent_map_from_wfl(source_wfl_pid) 	#we may want to get back count of items with freq > c/o

		if freq_token_count > 0 do
			process_collocations2(root_sent_map, root_sent_map, source_wfl_pid)
		else
			#this wfl has nothing in it, return the parent which will be the last wfl to have frequent tokens
			WFL.get_parent(source_wfl_pid)
		end
	end

	defp process_collocations2(sent_map, root_sent_map, source_wfl_pid) do
		cutoff = 2	#this will have to be in config or similar.
				
		{:ok, colloc_wfl_pid} = WFL.start_link(source_wfl_pid)
		Parallel.pjob(sent_map, [{Collocation, :pre_pair_up, [root_sent_map, colloc_wfl_pid]}])  #pass in new colloc_wfl_pid?  we can use that then to create a new sent_map.

		#pairs = Collocation.pair_up(sent_map, sent_map)
		#IO.inspect(pairs)
		##/sent_map

		process_collocations(colloc_wfl_pid)
	end


	def get_colloc_continuations(colloc_wfl_pid, continuation_wfl_pid) do
		# i think p_s_t is phrase something type
		cutoff = 1	#get from config
		colloc_types = WFL.get_wfl(colloc_wfl_pid).types		
		frequent_collocs = Enum.filter(colloc_types, fn({_,  wfl_type}) -> 
			_sample_colloc = {<<0, 0, 0, 93, 0, 0, 0, 183, 0, 0, 0, 101>>,
					 %{concretisations: [], freq: 1, instances: [{19, {0, 2}}],
					  is_common: false, type: <<0, 0, 0, 93, 0, 0, 0, 183, 0, 0, 0, 101>>,
					  type_id: <<0, 0, 1, 42>>}}


			#IO.inspect({11, wfl_type}) 
			wfl_type >= cutoff 
		end)	#note - this part iterates so we need to call a holding function.

		case frequent_collocs do
			[_h | _t] = frequent_collocs ->
				#we have at least one frequent colloc so process it
				###-{:ok, new_colloc_wfl_pid} = WFL.start_link(colloc_wfl_pid)

				Parallel.pjob(frequent_collocs, [{Collocation, :set_continuations, []}])
				##-sents = Stream.map(TokensBinary.get_map(), fn({sentence_id, _}) -> sentence_id end)
				###Parallel.pjob(sents, [{Collocation, :combine_phrases, [colloc_wfl_pid]}])
				###-Parallel.pjob(sents, [{Collocation, :x_phrases, [colloc_wfl_pid]}])
				##Parallel.pjob(sents, [{Collocation, :process_sent_map, [continuation_wfl_pid, colloc_wfl_pid, fn(s) -> Collocation.get_temp_x(s) end]}])
				m = TokensBinary.get_map()
			Collocation.process_sent_map(m, continuation_wfl_pid, colloc_wfl_pid, fn(sent_map, sent_id) -> Collocation.temp_get_x(sent_map, sent_id) end)
				
				###Parallel.pjob(frequent_collocs, [{Collocation, :do_phrase, [new_colloc_wfl_pid, continuation_wfl_pid]}])				
				###get_colloc_continuations(new_colloc_wfl_pid, continuation_wfl_pid)	#Is this this tail recursive? perhaps the catch all clause also needs to call the same function
			_ ->
				colloc_wfl_pid	
		end		
	end


	def do_phrase_wfls(nil, _source_wfl_pid, _deadend_wfl_pid) do
		:ok
	end

	def do_phrase_wfls(wfl_pid, wfl_pid, _deadend_wfl_pid) do
		##if we have reached the root wfl, then no more to do
		:ok
	end

	def do_phrase_wfls(phrase_wfl_pid, root_wfl_pid, deadend_wfl_pid) do
		## each phrase is concretisation of the set of phrases that is itself minus one token cat sat on -> {cat, [saton, _on sat]}

		{phrase_wfl, parent_wfl_pid} = WFL.get_wfl_state(phrase_wfl_pid)
				
		Parallel.pjob(phrase_wfl.types, [{Collocation, :do_concretisation, [phrase_wfl, root_wfl_pid, deadend_wfl_pid]}])		
	
		do_phrase_wfls(parent_wfl_pid, root_wfl_pid, deadend_wfl_pid)
	end

	def exp_token(token_id, wfl_pid, parent_wfl_pid, root_wfl_pid, to_text) do		
		phrase = xp_token([{token_id, wfl_pid, parent_wfl_pid}], root_wfl_pid, <<>>)
		if to_text == true do
			WFL.translate_phrase(root_wfl_pid, phrase)
		else
			Utils.rev_bin(phrase)
		end

	end


	def xp_token([], _root_wfl_pid, phrase) do 
		#we've finished if there are no more tokens to process for this chunk, and there are no remaining chunks left
		phrase
	end

	def xp_token([{token_id, _wfl_pid, parent_wfl_pid} | rest], root_wfl_pid, phrase) when is_nil(parent_wfl_pid) do 
		#this token_id is not expandable (points to real word)
		new_phrase = token_id <> phrase
		xp_token(rest, root_wfl_pid, new_phrase)
	end

	def xp_token([{token_id, wfl_pid, parent_wfl_pid} | rest], root_wfl_pid, phrase) do
		#this token_id is expandable
		{token_type, lhs_parent_wfl_pid} = WFL.get_token_from_id(wfl_pid, token_id)
		IO.inspect({:token_type, token_type, token_id})
		grandparent_wfl_pid = WFL.get_parent(parent_wfl_pid)
		<<lhs :: binary-size(4), rhs :: binary-size(4)>> = token_type

		new_stack = [{lhs, parent_wfl_pid, grandparent_wfl_pid} | [{rhs, root_wfl_pid, root_wfl_pid} | rest]]

		xp_token(new_stack, root_wfl_pid, phrase)
	end


	def expand_token({<<>>, _wfl_pid, _parent_wfl_pid}, continuation_wfl_pid, [next_chunk | rest_chunks], phrase, to_text) do 
		# we have processed a token chunk or phrase section - but there are more chunks to do. 
		expand_token(next_chunk, continuation_wfl_pid, rest_chunks, phrase, to_text)
	end

	def expand_token({<<token_ids :: binary>>, wfl_pid, parent_wfl_pid}, continuation_wfl_pid, next_chunks, phrase, to_text) when is_nil(parent_wfl_pid) do
		# this chunk cannot be expanded further so copy over to phrase - expecting this to originate from combinations wfl only and wfl_pid will now be as per root wfl.
		new_phrase = Utils.rev_bin4(token_ids, phrase) #prepended in reverse order onto phrase		
		case next_chunks do
			[next_chunk | rest_chunks] ->
				expand_token(next_chunk, continuation_wfl_pid, rest_chunks, new_phrase, to_text)
			[] ->
				expand_token({<<>>, wfl_pid, nil}, continuation_wfl_pid, [], new_phrase, to_text)
		end
		
	end

	def expand_token({<<lhs_token_id :: binary-size(4), rhs_token_id :: binary-size(4)>>, wfl_pid, parent_wfl_pid}, continuation_wfl_pid, next_chunks, phrase, to_text) do
		# token id here should now come from a wfl composed of lhs, rhs where rhs is from continuation_wfl and lhs is from a descendant of the combination_wfl
		
		{lhs, lhs_parent_wfl_pid} = WFL.get_token_from_id(wfl_pid, lhs_token_id)
		lhs_grandparent_wfl_pid = WFL.get_parent(lhs_parent_wfl_pid)

		{rhs, rhs_parent_wfl_pid} = WFL.get_token_from_id(continuation_wfl_pid, rhs_token_id)		
		rhs_grandparent_wfl_pid = WFL.get_parent(rhs_parent_wfl_pid)

		# the rhs has to go onto the stack yet to be processed
		new_chunk_stack = [{rhs.token_id, rhs_parent_wfl_pid, rhs_grandparent_wfl_pid} | next_chunks]

		# the lhs can already go onto the current chunk		
		expand_token({lhs.token_id, lhs_parent_wfl_pid, lhs_grandparent_wfl_pid}, continuation_wfl_pid, new_chunk_stack, phrase, to_text)
		
	end


	def expand_token({<<lhs_token_id :: binary-size(4), rhs_token_id_with_gap :: binary-size(4)>>, wfl_pid, parent_wfl_pid}, continuation_wfl_pid, next_chunks, phrase, to_text) do
		# token id here should now come from a wfl composed of lhs, rhs where rhs is from continuation_wfl and lhs is self-or-descendant of the combination_wfl
		
		{lhs, lhs_parent_wfl_pid} = WFL.get_token_from_id(wfl_pid, lhs_token_id)
		lhs_grandparent_wfl_pid = WFL.get_parent(lhs_parent_wfl_pid)

		<<gap_byte :: binary-size(1),  rhs_rest :: binary-size(3)>> = rhs_token_id_with_gap
		
		gap_count = :binary.decode_unsigned(gap_byte)

		rhs_token_id = <<0 :: integer-unit(8)-size(1)>> <> <<rhs_rest :: binary >>


		{rhs, rhs_parent_wfl_pid} = WFL.get_token_from_id(continuation_wfl_pid, rhs_token_id)		
		rhs_grandparent_wfl_pid = WFL.get_parent(rhs_parent_wfl_pid)

		# the rhs has to go onto the stack yet to be processed
		new_chunk_stack = [{rhs.token_id, rhs_parent_wfl_pid, rhs_grandparent_wfl_pid} | next_chunks]

		# the lhs can already go onto the current chunk		
		expand_token({lhs.token_id, lhs_parent_wfl_pid, lhs_grandparent_wfl_pid}, continuation_wfl_pid, new_chunk_stack, phrase, to_text)
		
	end

	def expand_token({<<lhs_token_id :: binary-size(4), rhs_token_id :: binary-size(4)>>, wfl_pid, parent_wfl_pid}, continuation_wfl_pid, next_chunks, phrase, to_text) do
		# token id here should now come from a wfl composed of lhs, rhs where rhs is from continuation_wfl and lhs is from a descendant of the combination_wfl
		
		{lhs, lhs_parent_wfl_pid} = WFL.get_token_from_id(wfl_pid, lhs_token_id)
		lhs_grandparent_wfl_pid = WFL.get_parent(lhs_parent_wfl_pid)

		{rhs, rhs_parent_wfl_pid} = WFL.get_token_from_id(continuation_wfl_pid, rhs_token_id)		
		rhs_grandparent_wfl_pid = WFL.get_parent(rhs_parent_wfl_pid)

		# the rhs has to go onto the stack yet to be processed
		new_chunk_stack = [{rhs.token_id, rhs_parent_wfl_pid, rhs_grandparent_wfl_pid} | next_chunks]

		# the lhs can already go onto the current chunk		
		expand_token({lhs.token_id, lhs_parent_wfl_pid, lhs_grandparent_wfl_pid}, continuation_wfl_pid, new_chunk_stack, phrase, to_text)
		
	end


	def expand_token(<<token_id :: binary-size(4)>>, wfl_pid, parent_wfl_pid, continuation_wfl_pid, next_chunks, phrase, to_text) do
		#we have a single lookup token - place token_id in current chunk and process - not sure that we would actually come here - check the api call
		IO.puts("Single token lookup")
		grandparent_wfl_pid = WFL.get_parent(parent_wfl_pid)
		new_chunk_stack = [{token_id, parent_wfl_pid, grandparent_wfl_pid}]
		expand_token({token_id, parent_wfl_pid, grandparent_wfl_pid}, continuation_wfl_pid, new_chunk_stack, phrase, to_text)
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
	


end
