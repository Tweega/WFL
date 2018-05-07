
defmodule WFL_Repo do

  def saveWFL()  do
      wfl_pid = X_WFL.get_pid_from_name("root_wfl_pid")
      if wfl_pid != nil do
    	  wfl_types = WFLScratch.Server.get_sorted_wfl(wfl_pid, :freq, :asc)

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

        corpus_name = WFLScratch.Server.getCorpusName
    		#possible to decorate the struct definitions to include only some fields - however we still need to change token_id and instances
    		#if we want to be able to reconstruct the entire wfl then we will have to encode and decode all fields
    		xx = %{wfl: %{stats: %{corpus_name: corpus_name, token_count: cumFreq, type_count: tokenCount}, types: wflTypes}, sentences: sentenceList}
    		{:ok, wfl_json} = Poison.encode(xx)
    		 #IO.inspect(wfl_json)

        #poison escapes quote marks so don't send as io list - build string or pass json is as parameter
        sql = "select corpus_id from create_corpus"

        #sql = "INSERT INTO public.corpora(corpus_name, wfl_jsonb) VALUES(?, ?)\"" <> corpus_name <> "\", " <> wfl_json <> ")"
        #IO.inspect(sql)

    		case PostgrexHelper.query(sql, [corpus_name, wfl_json, cumFreq, tokenCount]) do
    			:ok ->
    				IO.puts("saved #{corpus_name}")
    			%Postgrex.Error{} = pge ->
    				IO.inspect(pge)
    			other ->
    			IO.puts("Unexpected event during saving of corpus #{corpus_name}: #{other}")
    		end

    end
  end
end
