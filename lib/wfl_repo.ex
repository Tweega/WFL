
defmodule WFL_Repo do
  def saveAll(corpusName) do
      saveWFL(corpusName)
      saveCollocations(corpusName)
      saveConcretisations(corpusName)
  end

  def saveWFL(corpus_name) do
      wfl_pid = X_WFL.get_pid_from_name("root_wfl_pid")
      if wfl_pid != nil do
    	  wfl_types = WFLScratch.Server.get_sorted_wfl(wfl_pid, :freq, :asc)

        #IO.inspect(wfl_types)

        {wflTypes, {cumFreq, tokenCount}} =

          Enum.reduce(wfl_types, {[], {0, 0}}, fn ({k, %WFL_Type{freq: freq, type_id: type_id, instances: instances}}, {typesAcc, {cf, tc}}) ->

      			new_instances = Enum.map(instances, fn({sent_id, {first_off, last_off}}) ->
      					[sent_id, first_off,  last_off]
      			end)

      			t_id = Utils.binary_to_string(type_id)

            {[%{type: k, type_id: t_id, freq: freq, instances: new_instances} | typesAcc], {cf + freq, tc + 1}}
      		end)

    		sentenceMap = Sentences.get_map()


    		sentenceList = Enum.map(sentenceMap, fn ({sent_id, sent}) ->
    			%{sent_id: sent_id, sent: String.reverse(sent)}
    		end)
    		|> Enum.sort(fn(s1, s2) -> s1.sent_id < s2.sent_id end)

        #corpus_name = WFLScratch.Server.getCorpusName
    		#possible to decorate the struct definitions to include only some fields - however we still need to change token_id and instances
    		#if we want to be able to reconstruct the entire wfl then we will have to encode and decode all fields
    		xx = %{wfl: %{stats: %{corpus_name: corpus_name, token_count: cumFreq, type_count: tokenCount}, types: wflTypes}, sentences: sentenceList}
    		{:ok, wfl_json} = Poison.encode(xx)

        sql = "select corpus_insert($1::character varying, $2::jsonb, $3::integer, $4::integer)"

        #sql = "INSERT INTO public.corpora(corpus_name, wfl_jsonb) VALUES(?, ?)\"" <> corpus_name <> "\", " <> wfl_json <> ")"
        #IO.inspect(sql)

        case PostgrexHelper.query(sql, [corpus_name, wfl_json, cumFreq, tokenCount]) do
    			{:ok, _x} ->
    				IO.puts("saved #{corpus_name}")
    			%Postgrex.Error{} = pge ->
    				IO.inspect(pge)
    			other ->
    			IO.puts("Unexpected event during saving of corpus #{corpus_name}: #{other}")
    		end

    end
  end

  def saveSentences(corpus_name) do
  	#corpus_name = WFLScratch.Server.getCorpusName
  	#poison escapes quote marks so don't send as io list - build string or pass json is as parameter
    sql = "select sentences_insert($1::character varying(30), $2::jsonb)"

  	sentenceMap = Sentences.get_map()


  	sentenceList = Enum.map(sentenceMap, fn ({sent_id, sent}) ->
  		%{sent_id: sent_id, sent: String.reverse(sent)}
  	end)
  	|> Enum.sort(fn(s1, s2) -> s1.sent_id < s2.sent_id end)

  	{:ok, sentence_json} = Poison.encode(sentenceList)

  	case PostgrexHelper.query(sql, [corpus_name, sentence_json]) do
      {:ok, num_sents} ->
  			IO.puts("saved #{num_sents} sentences")
  		%Postgrex.Error{} = pge ->
  			IO.inspect(pge)
  		other ->
  			IO.puts("Unexpected event during saving of sentences #{corpus_name}: #{other}")
  	end

  end

  def saveCollocations(corpus_name) do
    wfl_pid = X_WFL.get_pid_from_name("root_wfl_pid")
    #corpus_name = WFLScratch.Server.getCorpusName
    #poison escapes quote marks so don't send as io list - build string or pass json is as parameter
    sql = "select collocations_insert($1::character varying(30), $2::character varying(30), $3::jsonb)"
    #perhaps we could look at a transaction for these
    types = WFL.get_wfl(wfl_pid).types
    Enum.each(types, fn({key, token_info}) ->
      instances = getRichInstances(token_info)
      collocMap = getCollocations(instances, wfl_pid)
      {:ok, colloc_json} = Poison.encode(collocMap)
        case PostgrexHelper.query(sql, [corpus_name, key, colloc_json]) do
          {:ok, _num_collocs} ->
            #IO.puts("saved #{key}")
            _z = 1
          %Postgrex.Error{} = pge ->
            IO.inspect(pge)
          other ->
            IO.puts("Unexpected event during saving of collocation #{key}: #{other}")
        end
      end)
      IO.puts("saved collocations")
  end

  def getRichInstances(tokenInfo) do
    #go through each concretisation {wfl_pid,  token_id} and get those instances
    concretisations = case tokenInfo.concretisations do   #better to initiate to MapSet.new?
      nil -> MapSet.new()
      concs -> concs
    end

    Enum.reduce(concretisations, MapSet.new(), fn (%Concretisation{pid: conc_pid, token_id: conc_token}, instances) ->
      #IO.inspect("hello")
      %WFL_Type{instances: concInstances} = WFL.get_token_info_from_id(conc_pid, conc_token)
      #IO.inspect({:instances, instances})
      Enum.reduce(concInstances, instances, fn ({sent_id, {first_off, last_off}}, instanceAcc) ->
        bin_tokens = TokensBinary.get_bin_tokens(sent_id)
      #	IO.inspect("how are you?")
      first_bytes = first_off * 4
      last_bytes = (last_off * 4) - first_bytes

      <<x::binary-size(first_bytes), phrase::binary-size(last_bytes), _rest::binary>> = bin_tokens

      token_list = for << b::binary-size(4) <- phrase>>, do: b

      keyIndex = Enum.reduce_while(token_list, first_off, fn (token_bin, index) ->
        {spacelessToken, spaceCount} = Utils.strip_space_x(token_bin)
        spaces = Utils.get_space_count(token_bin)
        if  spacelessToken != tokenInfo.type_id do
          {:cont, index + 1 + spaces }
        else
          {:halt, index + spaces}
        end
      end)

      MapSet.put(instanceAcc, {sent_id, keyIndex})
      end)
    end)
  end


  def getCollocations(instances, wfl_pid) do
  	# for each instance, get the sentence and extract the collocation
  	# at the moment we are just doing this for individual tokens.
  	# when we do this  for phrases, we will need to record finish also.

  	collocs =
  		instances
  			|> Enum.map(fn({sent_id, offset}) ->
          %TokensBinary{bin_tokens: bin_tokens, offset_map: offset_map} = TokensBinary.get_sent_info(sent_id)
  				token_list = for << b::binary-size(4) <- bin_tokens>>, do: b
  				getCollocs(token_list, offset, sent_id, offset_map, wfl_pid)
  			end)
  	#now sort by lhs_rev
  	lhs_sort = Enum.sort(collocs, fn(a, b) ->
  		a.lhs_sort <=  b.lhs_sort
  	end)

  	{lhs_num_sort, _counter} = List.foldl(lhs_sort, {[], 1}, fn (colloc_map, {collocs, counter}) ->
  		{[Map.put(colloc_map, :lhs_sort, counter) | collocs], counter + 1}
  	end)

  	lhs_num_sort |> Enum.reverse
  end

  def getCollocs(bin_tokens, offset, sent_id, offset_map, wfl_pid) do

    throwAwayCount = max(offset - 3, 0)
  	{_throwAway, keep} = Enum.split(bin_tokens, throwAwayCount)
  	{lhs, [key | rest]} = Enum.split(keep, offset - throwAwayCount)
    {key_token, _} = WFL.get_token_from_id(wfl_pid, key)

  	rhs =  Enum.slice(rest, 0, 3)

    first_off = throwAwayCount
    last_off = offset + length(rhs)

    first_sent_off = Map.get(offset_map, first_off)

    last_token_len =
      case Enum.at(rhs, -1) do
        nil ->
          String.length(key_token)

        last_token_id ->
          {last_token, _} = WFL.get_token_from_id(wfl_pid, last_token_id)
          String.length(last_token)
      end

    last_sent_off = Map.get(offset_map, last_off)
    final_sent_off = last_sent_off + last_token_len

  	lhs_token_list = Enum.map(lhs, fn(token_id) ->
  			{lhs_token, _} = WFL.get_token_from_id(wfl_pid, token_id)
  			lhs_token
  	end)

  	lhs_tokens = Enum.join(lhs_token_list, " ")

  	rhs_tokens = Enum.map_join(rhs, " ", fn(token_id) ->
  			{rhs_token, _} = WFL.get_token_from_id(wfl_pid, token_id)
  			rhs_token
  	end)

  	lhs_sort = Enum.reverse(lhs_token_list) |> Enum.join(" ")

  	%{key: key_token, sent_id: sent_id, lhs: lhs_tokens, rhs: rhs_tokens, lhs_sort: lhs_sort, first_off: first_sent_off, last_off: final_sent_off}

  end

  def saveConcretisations(corpus_name) do
  	#start with the main wfl and then process all concretisations,depth first
  	require_scenic_route()
    ditch_redundant_abstractions()
  	root_wfl_pid = X_WFL.get_pid_from_name("root_wfl_pid")
  	#we want to end upp with a list
  	#[{phrase: "hello", concs: [{phrase: "hello there", concs: []}]}]
  	#corpus_name = WFLScratch.Server.getCorpusName
  		#types = WFL.get_wfl(root_wfl_pid).types
  		wfl_types = WFLScratch.Server.get_sorted_wfl(root_wfl_pid, :freq, :desc) #should only have to sort this once
  		Enum.each(wfl_types, fn({token, wfl_type}) ->

  		if wfl_type.freq > 1 && wfl_type.concretisations != nil do
  			new_instances = Enum.map(wfl_type.instances, fn({sent_id, {first_off, last_off}}) ->
  					[sent_id, first_off,  last_off]
  			end)
  			concList = MapSet.to_list(wfl_type.concretisations)
  			childConcs = getConcretisations(concList, [])
  			concTree = [%{phrase: token, freq: wfl_type.freq, instances: new_instances, concs: childConcs}]
        sql = "select concretisations_insert($1::character varying(30), $2::character varying(30), $3::jsonb)"

  			case Poison.encode(concTree) do
  				{:ok, conc_json} ->
  					case PostgrexHelper.query(sql, [corpus_name, token, conc_json]) do
  						{:ok, num_concs} ->
  							_x = num_concs
  						%Postgrex.Error{} = pge ->
  							IO.inspect(pge)
  						other ->
  							IO.puts("Unexpected event during saving of concretisation #{token}: #{other}")
  					end
  				_ ->
  					IO.inspect({:error, concTree})
  			end
  		end
  	end)
  end

  def require_scenic_route() do
  	flag_expression_tree_nodes()
  	#where a tree provides more than one path to a concretisation, delete any path that is direct
  	#other -> other hand -> on the other hand
  	#other -> on the other hand  (this direct route is redundant)
  	#start with the parent of the last WFL
  	[_last_wfl_pid | wfl_chain] = Enum.reverse(X_WFL.get_wfl_chain())
  	remove_direct_paths(wfl_chain)
  end

  def remove_direct_paths([]) do
  	:ok
  end


  def remove_direct_paths([wfl_pid | rest]) do
  	#get a list of all types in this wfl that are part of the expression tree - have root_info.freq == -1
  	#no leapfrogging allowed.
  	# if B -> AB -> ABCD, then B -> ABCD is redundant
  	#IO.inspect({:remove, wfl_pid})

  	%WFL_Data{types: wfl_types} =  WFL.get_wfl(wfl_pid)
  	#for each type that is part of the expression tree, get a set of all destinations

  	yy = Enum.filter(wfl_types, fn {_key, wfl_type} ->
  		wfl_type.root_info.freq == -1 && wfl_type.concretisations != nil
  	end)

  	remove_leapfroggers(yy, wfl_pid)

  	remove_direct_paths(rest)
  end

  def remove_leapfroggers([], _wfl_pid) do
  	:ok
  end

  def remove_leapfroggers([{type_key, wfl_type} | rest], wfl_pid) do
  	#get descendants of this node's concretisaions
  	grandchildren = Enum.reduce(wfl_type.concretisations, [], fn (%Concretisation{pid: conc_pid, token_id: conc_token_id}, acc) ->
  			conc_info = WFL.get_token_info_from_id(conc_pid, conc_token_id)
  			case conc_info.concretisations do
  				nil ->
  					acc
  				_ ->
  					acc ++ Enum.to_list(conc_info.concretisations)
  					# Enum.reduce(conc_info.concretisations, acc, fn (%Concretisation{} = conc2, acc2)->
  					# 	#all i am doing here is joining two lists. Just use ++?
  					# 	[conc2 | acc2]
  					# end)
  			end
  	end)

  	descendants = get_concretisation_descendants(grandchildren, MapSet.new())
  	_ww = X_WFL.expand_type_id(wfl_pid, wfl_type.type_id, true)
  		#IO.inspect({:grandchildren,  grandchildren, :ww, ww, :descendants, descendants})

  	# we now want to see if any of the list of wfl_type.concretisations is in the descendant set
  	{rejects, filtered_concretisations} =
  		Enum.split_with(wfl_type.concretisations, fn (%Concretisation{} = conc) ->
  			 MapSet.member?(descendants, conc)
  		 end)

  	if rejects != [] do
  		#update this wfl type to reflect new concretisation list
  		dd = X_WFL.expand_type_id(wfl_pid, wfl_type.type_id, true)
  		#IO.inspect({:reject, rejects, :pid, wfl_pid, :dd, dd})

  		WFL.update_concretisations(wfl_pid, type_key, Enum.into(filtered_concretisations, MapSet.new()))
  	end

  	remove_leapfroggers(rest, wfl_pid)

  end

  def flag_expression_tree_nodes() do
  	root_wfl_pid = X_WFL.get_pid_from_name("root_wfl_pid")
  	#root_colloc_pid = get_root_colloc_pid()
  	cut_off = 2
  	sorted_types = WFLScratch.Server.get_sorted_wfl(root_wfl_pid, :freq, :desc) #should only have to sort this once - we should be streaming this too

  	concretisations = Enum.reduce_while(sorted_types, [], fn ({_key, wfl_item}, acc) ->
  		if wfl_item.freq < cut_off do
  			 {:halt, acc}
  		else
  			{:cont, [{root_wfl_pid, wfl_item.type_id} | acc]}
  		end
  	end)
  	#wfl_types now looks like a list of concretisations
  	flag_tree_nodes(concretisations)
  end

  def flag_tree_nodes([]) do
  	:ok
  end

  #we need the pid of the  type in order to update it.
  def flag_tree_nodes([{wfl_pid, type_id} | rest]) do
  	#this function should be on x_wfl as it crosses all WFLs
  	#set root info.freq to -1 to flag that this node is on the expression tree
  	wfl_type = WFL.flag_tree_node(wfl_pid, type_id)

  	#now process all the concretisations of this node
  	next_concretisations =
  		case wfl_type.concretisations do
  			nil ->
  				rest
  			_ ->
  				Enum.reduce(wfl_type.concretisations, rest, fn (%Concretisation{pid: conc_pid, token_id: conc_id}, acc) ->
  						[{conc_pid, conc_id} | acc]
  				end)
  		end
  	flag_tree_nodes(next_concretisations)
  end

  def getConcretisations([], phraseTree)  do
  	phraseTree
  end

  def getConcretisations([%Concretisation{pid: pid, token_id: token_id} | restConcs], phraseTree) do
  	#IO.inspect({:phraseTree, phraseTree})
  	phrase = X_WFL.expand_type_id_to_string(pid, token_id)
  	token_info = WFL.get_token_info_from_id(pid, token_id)
  	new_instances = Enum.map(token_info.instances, fn({sent_id, {first_off, last_off}}) ->
  			[sent_id, first_off,  last_off]
  	end)
  	concList =
  		case  token_info.concretisations do
  			nil ->
  				[]
  			concSet -> MapSet.to_list(concSet)
  		end
    childList = getConcretisations(concList, [])
  	getConcretisations(restConcs, [%{phrase: phrase, freq: token_info.freq, instances: new_instances, concs: childList} | phraseTree])
  end

  def get_concretisation_descendants([], descendants) do
  	descendants
  end

  def get_concretisation_descendants([%Concretisation{pid: conc_pid, token_id: conc_token_id} = conc | rest], descendants) do
  	new_descendants = MapSet.put(descendants, conc)
  	conc_info = WFL.get_token_info_from_id(conc_pid, conc_token_id)

  	child_descendants = case conc_info.concretisations do
  		nil ->
  			new_descendants
  		_ ->
  			get_concretisation_descendants(MapSet.to_list(conc_info.concretisations), new_descendants)
  	end
  	get_concretisation_descendants(rest, child_descendants)
  end





  def saveAll?(corpus_name) do
      saveWFL(corpus_name)
      #|> if_ok?(WFL_Repo.saveCollocations/0)
      #|> if_ok?(WFL_Repo.saveSentences/0)
      #|> if_ok?(WFL_Repo.saveConcretisations/0)
  end


    def if_ok?(ret_val, next_fun, args \\[]) do
      #For this to cope with save collocs we have to be able to either send in args
      ##or curry the sql arguments into a function with no params.
      {status, msg} =
        case ret_val do
            {:ok, x} ->
              {:ok, x}
            %Postgrex.Error{} = pge ->
              {:error, pge}
            other ->
              {:error, "Unexpected event"}
        end

      case status do
        {:error, errMsg} ->
          IO.inspect(errMsg)
        _ ->
          nil
        end

    end


    def ditch_redundant_abstractions() do
    		#this is different to the other remove_redundant_abstractions function
    		# here we want to remove abstractions that don't add anything to their single concretisation
    		# the information we need in this case is
    				#the count of concretisations for the abstraction,
    				# abstraction freq
    				# concretiser freq
    		#get list of root wfl types - presumably by this time we have flagged - maybe we could do this during the flagging
    		root_wfl_pid = X_WFL.get_pid_from_name("root_wfl_pid")
    		#root_colloc_pid = get_root_colloc_pid()
    		cut_off = 2
    		sorted_types = WFLScratch.Server.get_sorted_wfl(root_wfl_pid, :freq, :desc) #should only have to sort this once - we should be streaming this too

    		concretisations = Enum.reduce_while(sorted_types, [], fn ({_key, wfl_item}, acc) ->
    			if wfl_item.freq < cut_off do
    				 {:halt, acc}
    			else
    				{:cont, [{%Concretisation{pid: root_wfl_pid, token_id: wfl_item.type_id}, %Concretisation{pid: nil, token_id: nil}} | acc]}
    			end
    		end)

    		#wfl_types now looks like a list of concretisations
    		#IO.inspect(concretisations)
    		ditch_redundant_abstractions(concretisations)
    :ok
    end

    def ditch_redundant_abstractions([]) do
    	#IO.inspect("finished redundant abstractions")
    	:ok
    end


    def ditch_redundant_abstractions([{%Concretisation{pid: wfl_pid, token_id: type_id} = currentConc, parentConc} | rest]) do
    			#IO.inspect("hello")
    #	IO.inspect({:x, rest})
    	wfl_type = WFL.get_token_info_from_id(wfl_pid, type_id)
    	#IO.inspect(wfl_type)
    	cutoff = 2

    	# if wfl_type.concretisations != nil && length(MapSet.to_list(wfl_type.concretisations)) == 1 do
    	# 	IO.inspect(MapSet.to_list(wfl_type.concretisations))
    	# end


    	conc_list = case wfl_type.concretisations do
    		nil ->
    			[]
    		_ -> MapSet.to_list(wfl_type.concretisations)
    	end



    	next_concs =
    		case conc_list do
    			[] ->
    				rest
    			[%Concretisation{pid: child_conc_pid, token_id: child_conc_id} = childConc | []] ->
    				#IO.inspect(:hello)
    #IO.inspect(WFL.get_token_info_from_id(conc_pid, conc_id))

    				%WFL_Type{freq: conc_freq, concretisations: concs} = WFL.get_token_info_from_id(child_conc_pid, child_conc_id) #we might be able to perform this in one go
    				if parentConc.pid != nil && wfl_type.freq < conc_freq + cutoff do
    					#IO.inspect({:replace_concs, wfl_type.concretisations, concs, wfl_type.freq, conc_freq})
    					#my parent (parentConc) has a reference to me (currentConc).  it should replace that reference with a reference to my only child (conc_pid, conc_id)
    					WFL.replace_concs(parentConc, currentConc, childConc )
    					rest
    				else
    					[{childConc, currentConc} | rest]
    					# rest
    				end

    			_ ->
    				Enum.reduce(conc_list, rest, fn (conc, acc) ->
    						[{conc, currentConc} | acc]
    				end)
    				# rest
    		end
    		ditch_redundant_abstractions(next_concs)
    end

end
