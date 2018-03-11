defmodule WFLScratch.Server do
	use GenServer
	@name :WFL

	#API test change
	#we only have one of these? this creates a text reader for each file (could be handed an array of files or a wildcard file path)
	#and then collates all the results

	def processFile(filePath, readerModule) do
		:gen_server.cast(:WFL, {:wfl_file, {filePath, readerModule}})
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

	def handle_info( {:file_complete, wfl_pid}, state) do
		IO.puts "Handle info: File read: complete - next make a call into wfl to see what it has got."
		#mark grammar/common words - for the moment just using ["the", "a", "an"] - we should add these at the start.
		#if working with multiple files, create common tokens once and clone

		WFL.mark_common(wfl_pid, ["the", "a"])
		last_wfl_pid = process_collocations(wfl_pid)	#capturing last_wfl_pid only needed to allow us to keep it in scope after text has been processed so we ca interrogate from the command line

		X_WFL.start_link({wfl_pid, last_wfl_pid})

			{wfl_pid, colloc_pid, colloc_chain} = Collocation.expand_phrases()	#expand_phrases needs to include root_colloc
			Collocation.concretise_phrases()
		IO.inspect("finished")
		{:noreply, state}	end

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
		{root_sent_map, freq_token_count} = Collocation.create_sent_map_from_wfl(source_wfl_pid) 	#we may want to get back count of items with freq > c/o
		process_collocations2(root_sent_map, root_sent_map, source_wfl_pid)
	end


defp process_collocations2(sent_map, root_sent_map, source_wfl_pid, depth \\1) do
	IO.inspect(depth)
	cutoff = 2	#this will have to be in config or similar.
	Scratch.save_wfl(source_wfl_pid, "wfl_" <> Integer.to_string(depth) <> ".js")

		{:ok, colloc_wfl_pid} = WFL.start_link(source_wfl_pid)
		Parallel.pjob(sent_map, [{Collocation, :pre_pair_up, [root_sent_map, colloc_wfl_pid]}])  #pass in new colloc_wfl_pid?  we can use that then to create a new sent_map.

		{colloc_sent_map, freq_token_count} = Collocation.create_sent_map_from_wfl(colloc_wfl_pid) 	#we may want to get back count of items with freq > c/o
IO.inspect(freq_token_count: freq_token_count, depth: depth)
		if freq_token_count > cutoff && depth < 8 do #freq_token_count is the number of types whose frequency is > cutoff
		  process_collocations2(colloc_sent_map, root_sent_map, colloc_wfl_pid, depth + 1)
		else
			#this wfl has nothing in it, return the parent/source which will be the last wfl to have frequent tokens
			source_wfl_pid
		end
	end

end
