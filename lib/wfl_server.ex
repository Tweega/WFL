defmodule WFLScratch.Server do
	use GenServer
	@name :WFL

	#API test change
	#we only have one of these? this creates a text reader for each file (could be handed an array of files or a wildcard file path)
	#and then collates all the results

	def processFile(filePath, readerModule) do
		:gen_server.cast(:WFL, {:wfl_file, {filePath, readerModule}})
	end

	# def get_wfl(key, field \\ :freq, order \\ :desc) do
	# 	:gen_server.call(:WFL, {:get_wfl, key, field, order})
	# end
  #
	# def get_token_info(key, token) do
	# 	:gen_server.call(:WFL, {:get_token_info, key, token})
	# end

	# def get_wfl_stream(key) do
	# 	:gen_server.call(:WFL, {:get_wfl_stream, key})
	# end

	# def get_wfl_pid(key) do
	# 	:gen_server.call(:WFL, {:get_wfl_pid, key})
	# end
  #
	# def get_colloc_pid() do
	# 	:gen_server.call(:WFL, :get_colloc_pid)
	# end


	def start_link(_x) do 	#we could initialise with an existing wfl or lemma file? if so we could spawn the process that reads those in.
		:gen_server.start_link({:local, @name}, __MODULE__, %{}, [])
	end

	#Server
	def init(%{} = state) do
		#state is map with empty list of readers as its only element.
		{:ok, state}
	end

	def handle_cast( {:wfl_file, {filePath, readerModule}}, state) do
		{:ok, wfl_pid} = WFL.start_link()	#store this in map with filename as key? we need to have a completed status flag in state

		process_file(filePath, readerModule, wfl_pid)
		#should process_file be async?  or are we already async here because of cast
		#what is the effect of handle_info calls which set state before this call terminates?
		#we should probably make this a call, and run in separate process.

		{:noreply, state}
	end

	# def handle_call({:get_wfl_pid, key}, _client, state) do
	# 	#rename this to get_wfl_pid_from_key
	# 	wfl_pid = Map.get(state, key)
	# 	{:reply, wfl_pid, state}
	# end

	# def handle_call({:get_token_info, key, token}, _client, state) when is_pid(key) or is_atom(key) do
	# 	wfl_item = WFL.get_token_info(key, token)
	# 	{:reply, wfl_item, state}
	# end
  #
	# def handle_call({:get_token_info, key, token}, _client, state) do
	# 	wfl_pid = Map.get(state, key)
	# 	wfl_item = WFL.get_token_info(wfl_pid, token)
	# 	{:reply, wfl_item, state}
	# end

	# def handle_call({:get_wfl, key, field, order}, _client, state) when is_pid(key) or is_atom(key) do
	# 	sorted_wfl = get_sorted_wfl(key, field,  order)
  #
	# 	{:reply, sorted_wfl, state}
	# end
  #
	# def handle_call({:get_wfl, key, field, order}, _client, state) do
	# 	wfl_pid = Map.get(state, key)
	# 	sorted_wfl = get_sorted_wfl(wfl_pid, field,  order)
  #
	# 	{:reply, sorted_wfl, state}
	# end
  #


	def handle_info( {:file_complete, wfl_pid}, state) do
		IO.puts "Handle info: File read: complete - next make a call into wfl to see what it has got."
		#mark grammar/common words - for the moment just using ["the", "a", "an"] - we should add these at the start.
		#if working with multiple files, create common tokens once and clone

		WFL.mark_common(wfl_pid, ["the", "a"])
		last_wfl_pid = process_collocations(wfl_pid)	#capturing last_wfl_pid only needed to allow us to keep it in scope after text has been processed so we ca interrogate from the command line

		X_WFL.start_link({wfl_pid, last_wfl_pid})

			##!##{wfl_pid, colloc_pid, colloc_chain} = Collocation.expand_phrases()	#expand_phrases needs to include root_colloc
			###Collocation.check_expansions(colloc_chain) - i think this is only for debugging
			#while hapax are being freed, go through all the items in the expansion list and add to the concretisation sets
			#so lose_one(ABC) -> [AB, BC, AC].
			#look up each of the abstractions in the expansion map and add concretisation to their concretisation_map.
			###Collocation.konkret_machen()

			##!##Collocation.concretise_phrases()

		#new_state2 = Map.put_new(state, "last_wfl_pid", last_wfl_pid)
		IO.inspect("finished")
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
		PostgrexHelper.query("truncate table staging.phrase_candidate", [])
		q = PostgrexHelper.getAddPhraseQuery()
		{root_sent_map, freq_token_count} = Collocation.create_sent_map_from_wfl(source_wfl_pid) 	#we may want to get back count of items with freq > c/o
		process_collocations2(root_sent_map, root_sent_map, source_wfl_pid ,q)
	end


	defp process_collocations2(sent_map, root_sent_map, source_wfl_pid, add_phrase_query, depth \\1) do
		PostgrexHelper.query("truncate table staging.phrase_candidate", [])
		cutoff = 2	#this will have to be in config or similar.
		{:ok, num_released} = WFL.free_hapax(source_wfl_pid)
		Scratch.save_wfl(source_wfl_pid, "wfl_" <> Integer.to_string(depth) <> ".js")

		IO.inspect({:released, num_released})
		{:ok, colloc_wfl_pid} = WFL.start_link(source_wfl_pid)
		Parallel.pjob(sent_map, [{Collocation, :pre_pair_up, [root_sent_map, colloc_wfl_pid, add_phrase_query]}])  #pass in new colloc_wfl_pid?  we can use that then to create a new sent_map.
IO.inspect("creating sent map from wfl")
		{colloc_sent_map, freq_token_count} = Collocation.create_sent_map_from_wfl(colloc_wfl_pid) 	#we may want to get back count of items with freq > c/o
IO.inspect("DoNE  creating sent map from wfl: #{freq_token_count}")
cutoff = 1
		if freq_token_count > cutoff do #freq_token_count is the number of types whose frequency is > cutoff
			process_collocations2(colloc_sent_map, root_sent_map, colloc_wfl_pid, add_phrase_query, depth + 1)
		else
			#this wfl has nothing in it, return the parent/source which will be the last wfl to have frequent tokens
			##!##Process.exit(colloc_wfl_pid, :normal)
			##!##source_wfl_pid
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


end
