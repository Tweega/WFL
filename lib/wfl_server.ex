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

		process_file(filePath, readerModule, wfl_pid)

		{:noreply, state}
	end

	def handle_info( {:file_complete, wfl_pid}, state) do
		IO.puts "Handle info: File read: complete - next make a call into wfl to see what it has got."
		#mark grammar/common words - for the moment just using ["the", "a", "an"] - we should add these at the start.
		#if working with multiple files, create common tokens once and clone

		WFL.mark_common(wfl_pid, ["the", "a"])
		last_wfl_pid = process_collocations(wfl_pid)	#capturing last_wfl_pid only needed to allow us to keep it in scope after text has been processed so we ca interrogate from the command line

		X_WFL.start_link({wfl_pid, last_wfl_pid})

			_res = Collocation.expand_phrases()	#expand_phrases needs to include root_colloc
			##!##Collocation.concretise_phrases()
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
		{:ok, num_released} = WFL.free_hapax(source_wfl_pid) #pass in a save flag?
		IO.inspect({:hapax_released, num_released})
		{root_sent_map, _freq_token_count} = Collocation.create_sent_map_from_wfl(source_wfl_pid) 	#we may want to get back count of items with freq > c/o
		process_collocations2(root_sent_map, root_sent_map, source_wfl_pid)
	end


defp process_collocations2(sent_map, root_sent_map, source_wfl_pid, depth \\1) do

	#check that abstractions created in previous wfl are not redundant

	Scratch.save_wfl(source_wfl_pid, "wfl_" <> Integer.to_string(depth) <> ".js")

		{:ok, colloc_wfl_pid} = WFL.start_link(source_wfl_pid)
		Parallel.pjob(sent_map, [{Collocation, :pre_pair_up, [root_sent_map, colloc_wfl_pid]}])  #pass in new colloc_wfl_pid?  we can use that then to create a new sent_map.
		{:ok, num_released} = WFL.free_hapax(colloc_wfl_pid)
		IO.inspect({:released, num_released})
		remove_redundant_abstractions(colloc_wfl_pid)
		{colloc_sent_map, freq_token_count} = Collocation.create_sent_map_from_wfl(colloc_wfl_pid) 	#we may want to get back count of items with freq > c/o
IO.inspect(freq_token_count: freq_token_count, depth: depth)
		if freq_token_count > 1 && depth < 10 do
			# not interested in super long phrases.  May handle these later through some form of sentence subtraction
			#freq_token_count is the number of types whose frequency is > cutoff (actually cum freq of these - change tk)
			#may want to see if we can find an alternative to recursion here as we have a lot of data on stack.
			## Depends on whether this is tail recursive or not.  I would have thought it is though the else statement may rule that out. check tk

		  process_collocations2(colloc_sent_map, root_sent_map, colloc_wfl_pid, depth + 1)
		else
			#this wfl has nothing in it, return the parent/source which will be the last wfl to have frequent tokens
			source_wfl_pid
		end
	end

	def remove_redundant_abstractions(wfl_pid) do  #this could be on x_wfl as it involves two wfls
		#the aim of this function is to prevent the number of prospectove collocations getting out of hand in the event of
		#two long sentences being duplicated. It does this by looking at ABC, removing B to give A_C
		#then checking whether the freq. of A_C is any higher than ABC.
		#This only kicks in if frequency of A_C is at least 3, otherwise it is rejected in free_hapax

		wfls = X_WFL.get_chain(wfl_pid)
			|> Enum.reverse()
		{wfl, parent_wfl_pid} =
			case wfls do
				[_w, _p | []] ->
					{nil, nil}
				[w, p | _r] ->
					{w, p}
			_ ->
				{nil, nil}
			end
		redundant_abstractions =
			if wfl == nil do
				[]
			else
				cutoff = 2
				#for each type in current wfl
				wfl_types = WFL.get_wfl(wfl_pid).types
				{abstractions, concretisations} =
					Enum.reduce(wfl_types, {[], %{}}, fn {token, token_info}, {acc, concs} ->

						{lhs1, rhs1} = Utils.split_token(token)
						lhs1_token = WFL.get_token_info_from_id(parent_wfl_pid, lhs1).type  	#if we ever want to remove type from data, then will need to get all get_info functions to return key
						{lhs2, rhs2} = Utils.split_token(lhs1_token)

						rhs1_spaces = Utils.get_rhs_spaces(rhs1)
						rhs2_spaces = Utils.get_rhs_spaces(rhs2)
						intermediate_spaces = rhs1_spaces + rhs2_spaces

						abst_type = lhs2 <> Utils.set_spaces(rhs1, intermediate_spaces + 1)

						if intermediate_spaces < 2 && token_info.freq >= cutoff + intermediate_spaces do
							abstraction_type = lhs2 <> Utils.set_spaces(rhs1, intermediate_spaces + 1)

							new_concs = Map.update(concs, abstraction_type, {1, token_info.freq}, fn ({count, total}) ->
								{count + 1, total + token_info.freq}
							end)

							{[abstraction_type | acc], new_concs}
						else
							{acc, concs}
						end
					end)

				#index lhs for the current wfl
				#abstractions is a list, made from the current wfl of types with added spaces that should be present in parent wfl
				case abstractions do
					[_h | _t] ->
						#create index on lhs
						lhs_map = Enum.reduce(wfl_types, %{}, fn {k, v}, lhs_acc ->
							{lhs, _rhs} = Utils.split_token(k)
							Map.update(lhs_acc, lhs, [{k, v.type_id}], fn lhs_children ->
								[{k, v.type_id} | lhs_children]
							end)
						end)

#						IO.inspect(lhs_map)
#IO.inspect(abstractions)
						Enum.reduce(abstractions, [], fn (abstraction, reds) ->
							#find the abstraction in previous wfl
							{concretisation_count, concretisation_cum_freq} = Map.get(concretisations, abstraction)
							enough_concretisations = concretisation_count >= cutoff

							{is_redundant, redundant_id} = case enough_concretisations do
								true ->
									{false, nil}
								false ->

									abs_info = WFL.get_token_info(parent_wfl_pid, abstraction)   #this may not be found if limit on spaces
									case abs_info do
										nil ->
											{false, nil}
										_ ->
											{abs_info.freq < concretisation_cum_freq + cutoff, abs_info.type_id}
									end
							end

							if is_redundant do
	#							IO.inspect({:redundant, redundant_id})
								# get list of tokens in wfl where LHS == abstraction
								case Map.get(lhs_map, redundant_id) do
									nil -> reds
									abs ->
										[abs | reds]
								end
							else
								reds
							end
						end)
						|> List.flatten()
				_ ->
						[]
				end
			end

		{:ok, count_dropped} = WFL.drop_types(wfl_pid, redundant_abstractions)
		IO.inspect({:reds_dropped, count_dropped})
	end


end
