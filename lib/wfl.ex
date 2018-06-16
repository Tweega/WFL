#wfl_types.ex
defmodule OffsetMaps do
  defstruct([token_map: %{}, combination_map: %{}])
end
#end wfl_types.ex

defmodule WFL do
	#@defaults %WFL_Data{}	#set default text processing module here

	use GenServer

	#API
	def addToken(pid, %TokenInput{} = token_info) do
		:gen_server.call(pid, {:add_token, token_info })
	end

	def addColloc(pid, %TokenInput{} = token_info) do  	#defstruct([:token, :instance])		may not use this api
		:gen_server.call(pid, {:add_colloc, token_info })
	end

	def get_wfl(wfl_pid) do
		:gen_server.call(wfl_pid, :get_wfl)
	end

  def get_sorted_wfl(wfl_pid, field, order) do
		:gen_server.call(wfl_pid, {:get_sorted_wfl, field, order})
	end

	def get_wfl_state(wfl_pid) do
		:gen_server.call(wfl_pid, :get_state)
	end

	def get_token_info(wfl_pid, token) do
		:gen_server.call(wfl_pid, {:get_token_info, token})
	end

	def get_token_info_from_id(wfl_pid, token_id) do 	#can't distinguish between binary and string so have to make a different api call
		:gen_server.call(wfl_pid, {:get_token_info_from_id, token_id})
	end

	def get_token_from_id(wfl_pid, token_id) do 	#this returns {token, state} unlike get_token_info - may need to rename this.
		:gen_server.call(wfl_pid, {:get_token_from_id, token_id})
	end

	def get_parent(wfl_pid) do
		:gen_server.call(wfl_pid, :get_parent)
	end

	def mark_common(wfl_pid, type_list) do
		:gen_server.call(wfl_pid, {:mark_common, type_list})
	end

	def translate_phrase(wfl_pid, phrase) do
		:gen_server.call(wfl_pid, {:translate_phrase, phrase})
	end

  def add_concretisation(wfl_pid, phrase, is_phrase_id, concretisation_info, absSpaces, concSpaces) do
    #IO.inspect("wfl add concretisation")
		:gen_server.call(wfl_pid, {:add_concretisation, {phrase, is_phrase_id, concretisation_info, absSpaces, concSpaces}})
    #IO.inspect("wfl LEAVE concretisation")
	end

  def flag_tree_node(wfl_pid, phrase_id) do
		:gen_server.call(wfl_pid, {:flag_tree_node, phrase_id})
	end

  def update_concretisations(wfl_pid, phrase_type, concretisations) do
    :gen_server.call(wfl_pid, {:update_concretisations, phrase_type, concretisations})
  end

  def free_hapax(wfl_pid) do
    :gen_server.call(wfl_pid, {:free_hapax})
  end

  def drop_types(wfl_pid, redundant_types) do
    :gen_server.call(wfl_pid, {:drop_types, redundant_types})
  end

  def replace_concs(%Concretisation{pid: wfl_pid, token_id: typeID}, childConc, grandChildConc) do
      :gen_server.call(wfl_pid, {:replace_concs, typeID, childConc, grandChildConc})
    end

	def start_link(parent_wfl_pid \\ nil) do		#pass in an initial wfl?
		:gen_server.start_link(__MODULE__, {%WFL_Data{}, parent_wfl_pid}, [])
	end

	#Server
	def init(state) do
		#the state of a wfl is a WFL

		{:ok, state}
	end

	def handle_call(:get_state, _from, state) do
		{:reply, state, state}
	end

	def handle_call(:get_wfl, _from, {wfl, _parent} = state) do 	#why does this return the same value as get_state?
		{:reply, wfl, state}
	end

  def handle_call({:get_sorted_wfl, field, order}, _from, {%WFL_Data{} = wfl_data, _parent_wfl_pid} = state) do
    sorted_wfl = do_get_sorted_wfl(wfl_data.types, field, order)
    {:reply, sorted_wfl, state}
  end

	def handle_call(:get_parent, _from, {_wfl, parent} = state) do
		{:reply, parent, state}
	end

	def handle_call({:add_token, %TokenInput{token: token, instance: %TokenInstance{sentence_id: sentence_id, offset: offset}}}, _from, {%WFL_Data{} = wfl_data, parent_wfl_pid}) do

		{token_id, new_wfl} = process_token(token, sentence_id, offset, wfl_data, parent_wfl_pid)

		{:reply, {:ok, token_id}, {new_wfl, parent_wfl_pid}}
	end

	def handle_call({:get_token_info, token}, _client, {%WFL_Data{} = wfl_data, _parent_wfl_pid} = state) do
		wfl_item = fetch_token_info(wfl_data, token)
		{:reply, wfl_item, state}
	end

	def handle_call({:get_token_info_from_id, token_id}, _client, {%WFL_Data{} = wfl_data, _parent_wfl_pid} = state) do
		wfl_item = fetch_token_info_from_id(wfl_data, token_id)
		{:reply, wfl_item, state}
	end

	def handle_call({:get_token_from_id, token_id}, _client, state) do
		token_parent = fetch_token_from_id(state, token_id)
		{:reply, token_parent, state}
	end

	def handle_call({:mark_common, common_list}, _from, {wfl, parent}) do

		new_wfl_types = Enum.reduce(common_list, wfl.types, fn(common_token, wfl_types) ->
			Map.update(wfl_types, common_token, %WFL_Type{}, fn(wfl_info) ->
				%WFL_Type{wfl_info | is_common: true}
			end)
		end)

		{:reply, :ok, {%WFL_Data{wfl | types: new_wfl_types}, parent}}
	end

	def handle_call({:translate_phrase, phrase}, _client, {wfl, _parent} = state) do
		translation = translate_phrase(phrase, wfl, [])
		{:reply, translation, state}
	end

  def handle_call({:add_concretisation, concretisation_info}, _client, {%WFL_Data{} = wfl_data, parent_wfl_pid}) do
		#handle_cast?  we're not returning anything here.  we just need to know that the concretisation process is finished before we start using it.
		#this might be achieved by putting in a dummy call to WFL at the end which will only be processed when all other messages already on the stack are dealt with
#IO.inspect("Do we get here?")
		%WFL_Data{} = new_wfl = add_concretisation(wfl_data, concretisation_info)
    #IO.inspect("What about here?")
#IO.inspect("hello there how are you?")
    {:reply, :ok, {new_wfl, parent_wfl_pid}}
	end

  def handle_call({:flag_tree_node, phrase_id}, _client, {%WFL_Data{} = wfl_data, parent_wfl_pid}) do
    phrase_type = Map.get(wfl_data.type_ids, phrase_id)
    #IO.inspect({phrase_id, phrase_id})
    {updated_wfl_type, new_wfl_types} = Map.get_and_update!(wfl_data.types, phrase_type, fn %WFL_Type{root_info: root_info} = wfl_type ->
      new_root = %RootInfo{root_info | freq: -1}
      {wfl_type, %WFL_Type{wfl_type | root_info: new_root}}
    end)

    new_wfl = %WFL_Data{wfl_data | types: new_wfl_types}
    {:reply, updated_wfl_type, {new_wfl, parent_wfl_pid}}
  end

  def handle_call({:update_concretisations, phrase_type, new_concretisations}, _client, {%WFL_Data{} = wfl_data, parent_wfl_pid}) do
    new_wfl_types = Map.update!(wfl_data.types, phrase_type, fn %WFL_Type{} = wfl_type ->
      %WFL_Type{wfl_type | concretisations: new_concretisations}
    end)

    new_wfl = %WFL_Data{wfl_data | types: new_wfl_types}

    {:reply, :ok, {new_wfl, parent_wfl_pid}}
  end

  def handle_call({:free_hapax}, _client, {%WFL_Data{} = wfl_data, parent_wfl_pid}) do
    #aim is to set a different cutoff where lhs or rhs of first colloc is common
    #try calling out to get grandparent pid

    cutoff = 2
    {new_wfl_types, num_released} =
      Enum.reduce(wfl_data.types, {wfl_data.types, 0}, fn({token_key, wfl_type}, {types, n})  ->
        isCommon =
          if parent_wfl_pid == nil do
            false
          else
            gp = WFL.get_parent(parent_wfl_pid) # if this is a problem then we will need the colloc chain and parentage in named wfl
            if gp == nil do
              #this is first collocation wfl
              <<lhs :: binary-size(4), rhs_s :: binary-size(4)>> = token_key
              #IO.inspect({:qq, lhs, parent_wfl_pid})
              %WFL_Type{ is_common: lhs_common} = WFL.get_token_info_from_id(parent_wfl_pid, lhs)
              if lhs_common == true do
                true
              else
                rhs = Utils.strip_space(rhs_s)
                %WFL_Type{ is_common: rhs_common} = WFL.get_token_info_from_id(parent_wfl_pid, rhs)
                rhs_common
              end
              false
            else
              false
            end
          end

        cutoff =
          case isCommon do
            true -> 20
            false -> 2
          end

        rhs_spaces = case parent_wfl_pid do
          nil ->
            0
          _ ->
            Utils.get_space_count(token_key) #only require a phrase to account for most recently added spaces
        end
        if wfl_type.type == <<0, 0, 0, 9, 1, 0, 0, 19>> do
          IO.inspect({:spaces, wfl_type.type, rhs_spaces, wfl_type.freq, token_key})
        end
        #place an absolute limit on spaces? eg 4 - the time to impose wfl_type.spaces rule is when creating pairings in the first place
        if (wfl_type.freq < cutoff + rhs_spaces) || wfl_type.spaces > 4 do
          #if (wfl_type.freq < cutoff + rhs_spaces) do
          #if wfl_type.freq < cutoff do
          if wfl_type.type == <<0, 0, 0, 9, 1, 0, 0, 19>> do
            IO.inspect({:deleted, wfl_type.type})
          end

          new_types = Map.delete(types, token_key)
          {new_types, n + 1}
        else
          {types, n}
        end
      end)

    new_wfl = %WFL_Data{wfl_data | types: new_wfl_types}

    {:reply, {:ok, num_released}, {new_wfl, parent_wfl_pid}}
  end

  def handle_call({:drop_types, redundancies}, _client, {%WFL_Data{} = wfl_data, parent_wfl_pid}) do

    {redundant_types, redundant_ids, redundancy_count} = Enum.reduce(redundancies, {[], [], 0}, fn {red_type, red_id}, {red_types, red_ids, red_count} ->
      {[red_type | red_types], [red_id | red_ids], red_count + 1}
    end)

    new_types = Map.drop(wfl_data.types, redundant_types)
    new_ids = Map.drop(wfl_data.type_ids, redundant_ids)

    new_wfl = %WFL_Data{wfl_data | types: new_types, type_ids: new_ids}

    {:reply, {:ok, redundancy_count}, {new_wfl, parent_wfl_pid}}
  end

  def handle_call({:replace_concs, typeID, childConc, grandChildConc}, _client, {%WFL_Data{} = wfl_data, parent_wfl_pid}) do
      #the concretisations mapSet for typeID will change so that its reference to childConc is replaced by grandchildConc
      token = Map.get(wfl_data.type_ids, typeID)

      {current_wfl_type, new_wfl_types} = Map.get_and_update!(wfl_data.types, token, fn %WFL_Type{concretisations: concretisations} = wfl_type ->
        new_concretisations = MapSet.delete(concretisations, childConc)
          |> MapSet.put(grandChildConc)

        {wfl_type, %WFL_Type{wfl_type | concretisations: new_concretisations}}
      end)

      new_wfl = %WFL_Data{wfl_data | types: new_wfl_types}
      {:reply, current_wfl_type, {new_wfl, parent_wfl_pid}}

    end



	def handle_cast({:add_tokens, sentences}, {%WFL_Data{} = wfl_data, parent_wfl_pid}) do
		#this function is not part of the api for some reason. tk
		#at the end of this process
			#ok - all sentences in sentences should be saved somewhere keyed on sentence_id
			#ok  jut to be added to map - all tokens in a sentence should be merged into a binary format containing the token ids only, 4 bytes per token
			#ok the wfl type map should be updated so that there is wfl_info for each token with updated instances collection and frequency
			#ok the wfl_id map should be updated where there is a new token_type.

		#for each sentence get a sentence id and for each token get token id, and reconstruct sentence from that

		new_wfl_data = List.foldl(sentences, wfl_data, fn (%SentenceInfo{tokens: tokens, sentence: sentence}, wfl_data1) ->
			#get a sentence id
			sentence_id = SentenceCounter.get_sentence_id()

			#save the sentence somewhere - we don't need it for a while.  We could even store only offset and length in a filename bucket
			#this could/should be in a different process

            #sentence at this stage is back to front, but we can reverse that later for just the sentences that we need.
			Sentences.new(sentence_id, sentence)

			{_token_offset, tokens_binary, offset_map, wfl_data2} = process_tokens(tokens, sentence_id, wfl_data1, parent_wfl_pid)

			_sample_offset_map = %{0 => <<0, 0, 0, 4>>, 1 => <<0, 0, 0, 188>>, 2 => <<0, 0, 0, 158>>}

			#store tokens binary data - this will be the input 'text' for the next round of tokens.
			TokensBinary.new(sentence_id, %TokensBinary{bin_tokens: tokens_binary, offset_maps: %OffsetMaps{token_map: offset_map}})
			wfl_data2
		end)

		{:noreply, {new_wfl_data, parent_wfl_pid}}
	end


	defp process_tokens(tokens, sentence_id, %WFL_Data{} = wfl_data, parent_wfl_pid) do

		#we want to end up with
		#		defstruct([:type, :type_id, :freq, instances: []]) WFL_Type for each new token and with instances/freq update for existing types
		# we also want sentences of the form <<tok_id1 <> tok_id2>>...a binary string - so we are not storing the actual text here - should this be binary string or

		#we want a map of tokens so that we have [97, 123, 554, 98, 222, 27] etc. for e.g. the cat sat on the mat....except we want that as a binary string with 4 bytes per number
		#update / create WFL_Type for each token
		toke_offset = length(tokens) -1 # -1 for 1 based vs 0 based index.  tokens in reverse sentence order, start with count of words in sentence
		List.foldl(tokens, {toke_offset, <<"">>, %{}, wfl_data}, fn (token, {token_offset, tokens_binary, token_offset_map, wfl_data1}) ->
			{token_id, wfl_data2} = process_token(token, sentence_id, token_offset, wfl_data1, parent_wfl_pid)
			offset_map = Map.put(token_offset_map, token_offset, token_id)
			{token_offset - 1, << token_id <> tokens_binary >>, offset_map, wfl_data2}	#next accumulator value
		end)
	end

	defp process_token(token, sentence_id, offset, %WFL_Data{types: types, type_ids: type_ids}, parent_wfl_pid) do

		offsets = case offset do

			{_first_off, _last_off, _max_off} ->
				offset

			{_first_off, _last_off} ->
				offset

			first_off ->
				{first_off, first_off}
		end

		new_instance = {sentence_id,  offsets}

		#see if we already have this token in wfl
		{type_id, new_types} = Map.get_and_update(types, token, fn type_info ->
			new_type_info = case type_info do
				%WFL_Type{freq: freq, instances: instances} ->
					#existing type
					%WFL_Type{type_info | freq: freq + 1, instances: [new_instance | instances]}
				_ ->
					#token not seen before
					new_type_id = TokenCounter.get_token_id()
          space_count = case parent_wfl_pid do
            nil ->
              0
            _ ->
              {lhs, rhs} = Utils.split_token(token)
              rhs_spaces = Utils.get_rhs_spaces(rhs)
              lhs2 = WFL.get_token_info_from_id(parent_wfl_pid, lhs)
              rhs_spaces + lhs2.spaces
          end
					%WFL_Type{type: token, type_id: new_type_id, freq: 1, spaces: space_count, instances: [new_instance]}
			end
			{new_type_info.type_id, new_type_info}
		end)

		#we want to be able to look up type from id
		new_type_ids = Map.put_new(type_ids, type_id, token)

		{type_id, %WFL_Data{types: new_types, type_ids: new_type_ids}}
	end


	defp fetch_token_from_id({%WFL_Data{} = wfl_data, parent_wfl_pid}, token_id) do
		#returns the type denoted by a token id eg 123 -> cat, 234 -> sat, 222 -> [123, 234] or "cat sat".

		# we used to try searching up the tree in the event of not finding an item.
		#may need to end up doing this - but for now assuming that only need to search one level

		res = Map.get(wfl_data.type_ids, token_id)
		#do i always join things from the same wfl? - i think so - then we don't need deep search..
		{res, parent_wfl_pid}
	end


	defp fetch_token_info_from_id(wfl, token_id) do
		token = Map.get(wfl.type_ids, token_id)
		Map.get(wfl.types, token)
	end

	defp fetch_token_info(wfl, token) do
		Map.get(wfl.types, token)
	end


	def to_4_bytes(l) when is_integer(l) do
	 <<l :: integer-unit(8)-size(4)>>
	end

	def to_4_bytes(_l) do

	end

	defp add_concretisation(wfl, {phrase_id, is_phrase_id,
    %RootInfo{valid: concretiser_valid, freq: concretiser_freq,
      conc: %Concretisation{}} = concretiser, concSpaces, absSpaces}) do

    phrase_type = if is_phrase_id == true do
			Map.get(wfl.type_ids, phrase_id)
		else
			phrase_id
		end

    #IO.inspect({:Concretising, phrase_id, :with, concretisation_id, :is_phrase, is_phrase_id})
    #IO.inspect(phrase_type)
    ###Concretiser.new(phrase_type, self(),  concretiser_id, concretiser_pid)  #debug only
    #try - rescue block better than has_key??
		new_wfl_types =
      case Map.has_key?(wfl.types, phrase_type) do  #abstraction may not exist - some deleted when creating collocations
        false ->
          wfl.types
        true ->
        Map.update!(wfl.types, phrase_type, fn %WFL_Type{concretisations: concretisations} = wfl_type ->
  			#get_and_update returns a tuple of {the current value, and the value to be stored under the key, which in this case is phrase_type}
        #IO.inspect(concretisations)
        #we have the abstraction type thast we are going to concretise
        #do we have access to the root concretisation if it comes to use that instead of this one?
        #so if this abstraction is not more expressive, which concretising id do we use?
        abstractionFreq = wfl_type.freq

        #get the spacecount on the abstraction (and also on concretiser)

        cut_off = 1
        #{<<0, 0, 3, 32>>, true, %{conc: %Concretisation{pid: #PID<0.341.0>, token_id: <<0, 0, 3, 64>>}, freq: 2}}

        #if new_spaces (absSpaces > concSpaces) and not abs_freq > conc_freq, then flag this abstraction as ineligible as a concretiser
        # i think this is to stop cat _ mat concretising cat or mat, which will be concretised by cat sat, sat mat.  cat _ mat here adds nothing

        %RootInfo{} = conc_info =
          cond do
            absSpaces > concSpaces && !(abstractionFreq > concretiser_freq) ->
              %RootInfo{concretiser | valid: false}
            abstractionFreq > (concretiser_freq + absSpaces + cut_off) ->
              %RootInfo{valid: true, freq: abstractionFreq, conc: %Concretisation{pid: self(), token_id: wfl_type.type_id}}
            true ->
              concretiser
          end

        concMap = case concretisations do
          nil ->
            MapSet.new()
          _ ->
          concretisations
        end

        new_concretisations =
          if concretiser_valid == true do
            MapSet.put(concMap, concretiser.conc)
          else
            concMap
          end


  			%WFL_Type{wfl_type | concretisations: new_concretisations, root_info: conc_info}

      end)
    end

		%WFL_Data{wfl | types: new_wfl_types}
	end

	def translate_phrase(<<>>, _wfl, phrase) do
		phrase
	end


	def translate_phrase(<<token_id :: binary-size(4), rest :: binary>>, wfl, phrase) do
		<<space_count :: integer-unit(8)-size(1), token_bytes :: binary>> = token_id

		spaceless_token = <<0>> <> token_bytes

		#token_info = fetch_token_info_from_id(wfl, spaceless_token)
		token = Map.get(wfl.type_ids, spaceless_token)
		new_phrase = [token | phrase]
		phrase2 = case rest do
			<<>> -> new_phrase
			_ -> space_out(new_phrase, space_count)

		end
		translate_phrase(rest, wfl, phrase2)
	end

	def space_out(phrase, space_count) when space_count < 1 do
		phrase
	end

	def space_out(phrase, space_count) do
		space_out([<<"_">> | phrase], space_count - 1)
	end


  defp do_get_sorted_wfl(wfl_types, field, order) do #this should be on wfl
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

end
