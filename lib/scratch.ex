defmodule Scratch do
require Logger
#to do - review use of <> which is equiv to ++ .  Use <<a, b>> form where possible
use Bitwise

def new()  do
	tokens = Map.new()
	cat = to_4_bytes([87])	#tokens will have a binary id made up of 4 byte integers representing tokens

	tokens = Map.put(tokens, "cat", cat)	#lookup for token ids form token
	#we will need anoter dictionary keyed on tokenID

	token_events = Map.new()
	#structure is {sentenceID, tokenOffset}


	token_events = Map.put(token_events, cat, [{"s1", 3}, {"s2", 2}, {"s3", 1}, {"s4", 0}, {"s5", 6}])	#cat = <<0, 0, 0, 87>>

	sent_tokens = Map.new()

	sent_tokens = Map.put(sent_tokens, "s1", to_4_bytes([97, 21, 67, 87, 22, 33, 44]))
	sent_tokens = Map.put(sent_tokens, "s2", to_4_bytes([197, 121, 87, 871, 122, 331, 144]))
	sent_tokens = Map.put(sent_tokens, "s3", to_4_bytes([917, 87, 617, 817, 212, 313, 414]))
	sent_tokens = Map.put(sent_tokens, "s4", to_4_bytes([87, 211, 671, 187, 221, 133, 441]))
	sent_tokens = Map.put(sent_tokens, "s5", to_4_bytes([2971, 2211, 12321, 2187, 2221, 1233, 87]))

	{tokens, token_events, sent_tokens}
end

def to_4_bytes(l) do
	Enum.reduce(Enum.reverse(l), <<>>, fn(t, acc) -> << <<t :: integer-unit(8)-size(4)>> <> acc >> end)
end



def mask_tokens(<<num :: binary>>, mask) do
	#num is concatenation of token_ids as in <<0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 3>>
	#mask is a list of 1s and 0s as in [1, 0, 1] which would turn off the middle token id giving <<0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 3>>
	num_bytes = byte_size(num)
	<<x :: integer-unit(8)-size(num_bytes)>> = num
	mask_bin = mask_to_bits(mask)
	nb = byte_size(mask_bin)
	<<y :: integer-unit(8)-size(nb)>> = mask_bin
	masked_bin = x &&& y
	new_toke = :binary.encode_unsigned(masked_bin)
	n_bytes = num_bytes - byte_size(new_toke)
	#we now have to prepend zeros so that we still have num_bytes of binary
	<<0x00 :: integer-unit(8)-size(n_bytes)>> <> <<new_toke :: binary>>
end

def mask_to_bits(mask) do
	Enum.reduce(Enum.reverse(mask), <<>>, fn(bit, acc) ->
		if bit == 0 do
			<<0x00 :: integer-unit(8)-size(4)>> <> <<acc :: binary>>
		else
			:erlang.list_to_binary(List.duplicate(0xFF, 4)) <> <<acc :: binary>>
		end
	end)
end



def maskbits(bits, mask) do
	rev_bin(maskbits(bits, mask, <<>>))
end

def maskbits(_bits, [], acc) do
	acc
end


def maskbits(<<int :: binary-size(1), bits_rest :: binary>>, [mask | mask_rest], acc) do

	acc2 =
		if mask == 0 do
			<< <<0>> <> acc >>
		else
			<< <<int :: binary-size(1) >> <> acc >>
		end

	maskbits(bits_rest, mask_rest, acc2)

end


def combine_list(list) do
	combine_list(list, [[]])
end


def combine_list([], acc) do
	acc
end


def combine_list([h | t], acc) do
	new_acc = Enum.reduce(acc, acc, fn(item, accum) ->
		[[h | item] | accum]
	end)
	combine_list(t, new_acc)
end

def choose_n(list, n) do
	choose_n(list, {[[]], []}, n - 1)
end

def choose_n([], {_acc, result}, _n) do
	result
end

def choose_n([h | t], {acc, _result} = xx, n) do
	new_acc  = Enum.reduce(acc, xx, fn(item, {accum, res}) ->
		ac = if length(item) < n do
			[[h | item] | accum]
		else
			accum
		end

		rs = if length(item) == n do
			[[h | item] | res]
		else
			res
		end
		{ac, rs}
	end)
	choose_n(t, new_acc, n)
end

def rev_bin4(bin) do
	rev_bin4(bin, <<>>)
end

def rev_bin4(<<>>, acc) do
	acc
end

def rev_bin4(<<byte4 :: binary-size(4), rest :: binary>>, acc) do
	new_acc = << <<byte4 :: binary>>, <<acc :: binary>> >>
	rev_bin4(rest, new_acc)
end



def rev_bin(<< t :: binary >>) do
	rev_bin(t, <<>>)
end

def rev_bin(<<>>, store) do
	store
end

def rev_bin(<< h :: binary-size(1),  t :: binary >>, store) do
	#to convert number into binary :binary.encode_unsigned gives you smallest number of bytes for number
	#while <<256 :: size(16)>> gives you <<1, 0>>
	#and <<k :: integer-size(16) , rest :: binary>> = <<256 :: size(16)>> puts 256 into k

	rev_bin(t, <<h <> store>>)

end

def lose_one(list) do
	lose_one(list, [], [])
end

def lose_one([], _list2, acc) do
	acc
end


def lose_one([h | t], list2, acc) do
	new_acc = [Enum.reverse(list2) ++ t | acc]
	lose_one(t, [h | list2], new_acc)
end

def lose_one_bin(bin4) do
	#returns a list of binaries each one token shorter than the initial input binary

	bs = byte_size(bin4)
	if bs < 5 do
		[]
	else
		lose_one_bin(bin4, <<>>, [])
	end
end

def lose_one_bin(<<>>, _bin2, acc) do
	acc
end


def lose_one_bin(<<byte4 :: binary-size(4), rest :: binary>>, bin2, acc) do

	size_bin = byte_size(bin2)
	size_rest = byte_size(rest)
	#space_count are spaces before the owning token.  i.e. I am so many spaces plus 'cat'
	#phrase-less-one - for all instances other than the first, when bin2 is empty, we need to add a space to the first of rest.
	#this additional space should only apply once - so should not be part of the rest that is passed on in recursion..

	{new_bin, new_rest} =
		if size_bin == 0 do
			{bin2, rest}
		else
			rev_b = rev_bin4(bin2)
			if size_rest == 0 do
				{rev_b, rest}
			else
				<<space_count :: integer-unit(8)-size(1), rest2 :: binary>> = rest
				spaced_rest = << space_count + 1 >> <> rest2
				{rev_b, spaced_rest}
			end

		end

	new_acc = [<< new_bin :: binary, new_rest :: binary >> | acc]
	lose_one_bin(rest, << byte4 :: binary, <<bin2 :: binary>> >>, new_acc)
end

def go_wide(list) do
	#not sure what this was for or what wide refers to
	#FROM [1,2,3]
	#returns [[[2], [3]], [[1], [3]], [[1], [2]], [[1, 2], [1, 3], [2, 3]]]
	go_wide([list], [])
end


def go_wide([], acc) do
	IO.inspect(acc)
	acc
end

def go_wide([[] | t], acc) do
	go_wide(t, acc)
end


def go_wide([h|t], acc) do
	x = lose_one(h, [], [])
	{new_list, new_acc} = case x do
		[[]] ->
			{t, acc}
		_ ->
			{x ++ t, [x | acc]}
	end

	go_wide(new_list, new_acc)
end


def chew_bytes(<<bytes :: binary>>) do
	chew_bytes(bytes, [])
end

def chew_bytes(<<>>, acc) do
	acc
end


def chew_bytes(<<_byte4 :: binary-size(4), rest :: binary>>, acc) do
	chew_bytes(rest, [:a | acc])
end


def deep_lookup(lookup) do
	deep_lookup(lookup, [])
end

def deep_lookup([], acc) do
	acc
end

def deep_lookup([h | t], acc) do

	case h do
		1 ->
			IO.inspect(t)

			deep_lookup(t, [h | acc])
		_ ->
			deep_lookup([h - 1 | [1 | t]], acc)
	end
end


def pair_up(lhs_phrase_map, continuation_map) do

_sample_continuation_map = %{0 => [{0, <<0, 0, 0, 93>>, "we"}],
    1 => [{1, <<0, 0, 0, 92>>, "needed"}],
    2 => [{2, <<0, 0, 0, 7>>, "the"}],
    3 => [{3, <<0, 0, 0, 33>>, "perfect"}],
    4 => [{4, <<0, 0, 0, 88>>, "performance"}],
    5 => [{5, <<0, 0, 0, 42>>, "and"}],
    6 => [{6, <<0, 0, 0, 29>>, "it"}],
    7 => [{7, <<0, 0, 0, 41>>, "was"}],
    8 => [{8, <<0, 0, 0, 4>>, "one"}],
    9 => [{9, <<0, 0, 0, 75>>, "of"}],
    10 => [{10, <<0, 0, 0, 7>>, "the"}],
    11 => [{11, <<0, 0, 0, 162>>, "best"}],
    12 => [{12, <<0, 0, 0, 93>>, "we"}],
    15 => [{15, <<0, 0, 0, 8>>, "in"}]}

	cutoff = 2  	#+ 1?

	Enum.reduce(lhs_phrase_map, [], fn({lhs_first_off, lhs_phrases}, pair_acc) ->
		Enum.reduce(lhs_phrases, pair_acc, fn({lhs_last_off, lhs_token_id, _}, pairs)->
			Enum.reduce_while(1..cutoff + 1, pairs, fn (gap, pairs3) ->
				rhs_first_off = lhs_last_off + gap
				if cutoff + 1 < rhs_first_off - lhs_last_off do
					{:halt, pairs3}
				else
					case Map.fetch(continuation_map, rhs_first_off) do
						{:ok, rhs_continuations} ->
							Enum.reduce(rhs_continuations, pairs3, fn({rhs_last_off, rhs_token_id, _}, pairs4)->
								{:cont, [{lhs_token_id <> rhs_token_id, {lhs_first_off, rhs_last_off}} | pairs4]}
							end)
						_ ->
							{:cont, pairs3}
					end
				end
			end)
		end)
 	end)
end

def test_wfl_stream(wfl_pid) do
	file = File.stream!("wfl.txt")

	WFL.get_wfl(wfl_pid).types
	#s = WFLStream.get_wfl_stream(wfl_pid)
	#Stream.map(WFL.get_wfl(wfl_pid).types, fn (k) ->
	#after all this faffing around with streams - I actually want to sort the wfl by freq.
	Stream.map(WFL.get_wfl(wfl_pid).types, fn ({k, v}) ->
		IO.inspect(v)
		#{}"{" <> k <> ":" <> "\"" v.freq <> "}"
		"{#{k}: #{v.freq}}"
	end) |> Enum.into(file)
	#File.close(file)

end


def save_tokens([token | tokens], f) do
  save_token(token, f, false)
  #IO.inspect({:instances, token.instances})
  #{_k, info} = token
  save_rest(tokens, f)
end

def save_rest(tokens, f) do
  Enum.each(tokens, fn (x) ->
#IO.puts("Hello")
  #"{#{k}: #{v.freq}}"
#    IO.inspect(v)
    save_token(x, f)
  end )

end

def save_token({k, v}, file, leading_comma \\true) do

	token = Utils.binary_to_string(k)
	type_id = Utils.binary_to_string(v.type_id)
	iolist = [?}]

	iolist2 = save_sent_offs(v.instances, iolist)
	iolist3 =
		if v.is_common == true do
			[?,, 32, 34, "common", 34, ?:, "#{1}" | iolist2]
		else
			iolist2
		end

  a = [123, 34, "type", 34, ?:, 34, token, 34, ?,, 34, "type_id", 34, ?:, 34, type_id, 34, ?,, 34, "freq", 34, ?:, "#{v.freq}", 10, 13]

  aa = case leading_comma do
    false ->
      a
    _ ->
      [?, | a]

  end

	iolist4 = [aa | iolist3]
	#iolist4 = [?S | iolist3]
  IO.binwrite file, iolist4

end

def save_sent_offs([sent | sents], iolist) do
  #wrapper for sents here

  _bb = [?,, 34, "sentences", 34, ?:, ?[]
  ##IO.binwrite f, bb
  iolist2 = save_sent_off(sent, [?] | iolist], false)
  iolist3 = save_rest_sent_offs(sents, iolist2)
	[[?,, 34, "sentences", 34, ?:, ?[] | iolist3]
  #cc = [?]]
  #IO.binwrite f, cc
end

def save_rest_sent_offs(sents, iolist) do
	Enum.reduce(sents, iolist, fn(sent, acc) ->
    save_sent_off(sent, acc)
  end)
end

def save_sent_off({s, {o1, o2}}, iolist, leading_comma \\true) do

#b = [?,, 34, "sentences", 34, ?:, 32, ?[, ?[, "#{10}", ?,, "#{2}", ?], ?], 125]
#{2, {30, 30}} --> {"s":2, "o":[30,30]}
b = [?[, "#{s}", ?,, "#{o1}", ?,, "#{o2}", ?]]

  bb = case leading_comma do
    false ->
      b
    _ ->
      [b | [?,]]

  end
  [bb | iolist]
end

def save_sentences_containing_token(token) do
	root_wfl_pid = X_WFL.get_pid_from_name("root_wfl_pid")
	{:ok, _file} = File.open "sent_tokens.txt", [:write]
	token_info = WFL.get_token_info(root_wfl_pid, token)
	file_name = token <> "_sentences.txt"
	{:ok, file} = File.open file_name, [:write]
	token_info.instances
		|> Enum.map(fn({sent_id, _}) ->
			sent_id
		end)
		|> Enum.uniq
		|> Enum.each(fn(s_id) ->
				s = Sentences.get(s_id)
				s_io = [String.reverse(s), 10, 13]
				IO.binwrite(file, s_io)
			end)
	File.close(file)
end


def save_sentences_to_file() do
	file_name = "sentences.txt"
	s = Sentences.get_stream()
	{:ok, file} = File.open file_name, [:write]
	s
		|> Enum.each(fn({_k, v}) ->
				s_io = [String.reverse(v), 10, 13]
				IO.binwrite(file, s_io)
			end)
	File.close(file)
end


def save_wfl_to_iolist(wfl_pid) do
#once we have a sorted main wfl - may be an idea to hang onto it until done
#also - check where we get rid of hapax legomena.

#would it not be much simpler to create an object with profle that you want,
#and then get Poison to create the json?


_ioresult = [?], ?}]

  _types = WFL.get_wfl(wfl_pid).types
  wfl_types = WFLScratch.Server.get_sorted_wfl(wfl_pid, :freq, :asc)
# wfl_types2 = Enum.filter(wfl_types, fn({k,v}) ->
# 	case k do
# 		"hand" -> true
# 	#	"the" -> true
# 		"other" -> true
# 	#	"on" -> true
# 		_ -> false
# 	end
# end)
	#reduce the wfl list to an iolist - an array for each wfl type
	{io_json, _token_count} = Enum.reduce(wfl_types, {[], 0}, fn ({key, token_info}, {token_acc, token_count}) ->
		token_json = token_io(key, token_info, token_count > 0)
		# #reduce the sentence offset list to an iolist
		# {offset_json, _offset_count} = Enum.reduce(token_info.instances, {[], 0}, fn (instance, {inst_acc, inst_count}) ->
		# 		x = sent_off_io(instance, inst_count > 0)
		# 		{[x | inst_acc], inst_count + 1}
		# end)
		{[token_json | token_acc], token_count + 1}
	end)
	wfl_io = [?{, 34, "wfl", 34,   ?:, ?[, io_json, ?], ?}]

	#wfl_io = [?{, 34, "wfl", 34,   ?:, ?[, ?], ?}]
	IO.inspect(wfl_io)

	file_name = "wfl.txt"
	{:ok, file} = File.open file_name, [:write]
	IO.binwrite(file, wfl_io)
	File.close(file)
  #intention now is to save this to postgres wfl db
	corpus_name = WFLScratch.Server.getCorpusName

	#case PostgrexHelper.query(["INSERT INTO public.corpora(corpus_name, wfl_jsonb) VALUES(", 39, corpus_name, 39, ?,, 39, wfl_io, 39, ");"], []) do
	#case PostgrexHelper.query(["INSERT INTO public.corpora(corpus_name, wfl_jsonb) VALUES(", 39, "testIn", 39, ?,, 39, [?{, 34, "a", 34, 58, 34, "2", 34, ?}], 39, ");"], []) do
	case PostgrexHelper.query(["INSERT INTO public.corpora(corpus_name, wfl_jsonb) VALUES(", 39, corpus_name, 39, ?,, 39, wfl_io, 39, ");"], []) do
		:ok ->
			IO.puts("saved #{corpus_name}")
		%Postgrex.Error{} = pge ->
			IO.inspect(pge)
		other ->
		IO.puts("Unexpected event during saving of corpus #{corpus_name}: #{other}")
	end

end

def token_io(k, v, trailing_comma) do

	token = Utils.binary_to_string(k)
	type_id = Utils.binary_to_string(v.type_id)
	iolist = case trailing_comma do
		true -> [?},?,]
		false -> [?}]
	end


	iolist2 = save_sent_offs_io(v.instances, iolist)
	iolist3 =
		if v.is_common == true do
			[?,, 32, 34, "common", 34, ?:, "#{1}" | iolist2]
		else
			iolist2
		end

  a = [123, 34, "type", 34, ?:, 34, token, 34, ?,, 34, "type_id", 34, ?:, 34, type_id, 34, ?,, 34, "freq", 34, ?:, "#{v.freq}", 10, 13]

	[a | iolist3]
end


def sent_off_io({s, {o1, o2}}, leading_comma \\true) do

#b = [?,, 34, "sentences", 34, ?:, 32, ?[, ?[, "#{10}", ?,, "#{2}", ?], ?], 125]
#{2, {30, 30}} --> {"s":2, "o":[30,30]}
b = [?[, "#{s}", ?,, "#{o1}", ?,, "#{o2}", ?]]

  case leading_comma do
    false ->
      b
    _ ->
      [b | [?,]]
  end

end



def save_wfl(wfl_pid, file_name \\"wfl.txt") do
#once we have a sorted main wfl - may be an idea to hang onto it until done
#also - check where we get rid of hapax legomena.
  {:ok, file} = File.open file_name, [:write]
IO.binwrite file, [?{, 34, "wfl", 34,   ?:, ?[]
  #types = WFL.get_wfl(wfl_pid).types
  wfl_types = WFLScratch.Server.get_sorted_wfl(wfl_pid, :freq, :desc)
save_tokens(wfl_types, file)

  IO.binwrite file, [?], ?}]

  File.close(file)
end

def save_sentence_tokens() do
	#at the moment not filtering out non-rich sentences
	#saving only tokens not punctuation, as an array of tokens
	#sentences are stored where?

	root_wfl_pid = X_WFL.get_pid_from_name("root_wfl_pid")
	{:ok, file} = File.open "sent_tokens.txt", [:write]


	#we need to get hold of root_wfl_pid - we will not be passed this in
	IO.binwrite file, [?[]

	ss = TokensBinary.get_stream()

	Enum.reduce(ss, 0, fn({sent_id, %TokensBinary{bin_tokens: bt }}, item_count) ->

		sx = WFL.translate_phrase(root_wfl_pid, bt)
		sent = Sentences.get(sent_id)
			|> String.reverse()

		iolist = Enum.reduce(sx, [?], ?,, 34, "sent", 34, ?:, 34, "#{sent}", 34, ?}], fn(token, acc) ->
			[[?,, 34, token, 34] | acc]
		end)
		[h | t] = iolist
		[_comma | rest_head] = h
		iolist2 = [rest_head | t]
		iolist3 = [[?{, 34, "id", 34, ?:, "#{sent_id}", ?,, 34, "tokens", 34, ?:, ?[] | iolist2]

		iolist4 = case item_count do
			0 ->
				iolist3
			_ ->
			[?, | iolist3]
		end

		IO.binwrite file, iolist4
		item_count + 1
	end)
		IO.binwrite file, [?]]
	  File.close(file)
end


def save_sentences_with_punctuation() do
	ss = Sentences.get_stream()
	#sentences need reversing
	#save this alongside tokens?

	Enum.each(ss, fn(s) ->
		IO.inspect(s)   #{24, ".eerht owt eno"}
	end)
end


def save_concs() do
	#start with the main wfl and then process all concretisations,depth first
	WFL_Repo.require_scenic_route
	root_wfl_pid = X_WFL.get_pid_from_name("root_wfl_pid")
	#we want to end upp with a list
	#[{phrase: "hello", concs: [{phrase: "hello there", concs: []}]}]
	corpus_name = WFLScratch.Server.getCorpusName
		#types = WFL.get_wfl(root_wfl_pid).types
		wfl_types = WFLScratch.Server.get_sorted_wfl(root_wfl_pid, :freq, :desc) #should only have to sort this once
		Enum.each(wfl_types, fn({token, wfl_type}) ->

		if wfl_type.freq > 1 && wfl_type.is_common != true && wfl_type.concretisations != nil do
			new_instances = Enum.map(wfl_type.instances, fn({sent_id, {first_off, last_off}}) ->
					[sent_id, first_off,  last_off]
			end)
			concList = MapSet.to_list(wfl_type.concretisations)
			childConcs = getConcretisations(concList, [])
			concTree = [%{phrase: token, freq: wfl_type.freq, instances: new_instances, concs: childConcs}]
			sql = "insert into public.concretisations(corpus_name, token_id, conc_jsonb) VALUES ($1::character varying(30), $2::character varying(30), $3::jsonb)"
			case Poison.encode(concTree) do
				{:ok, conc_json} ->
					case PostgrexHelper.query(sql, [corpus_name, token, conc_json]) do
						:ok ->
							IO.puts("saved #{token}")
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



def getConcretisations([], phraseTree)  do
	phraseTree
end

def getConcretisations([%Concretisation{pid: pid, token_id: token_id} | restConcs], phraseTree) do
	IO.inspect({:phraseTree, phraseTree})
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


def save_concretisation_tree() do
	#start with the main wfl and then process all concretisations,depth first
	cutoff = 1
	root_wfl_pid = X_WFL.get_pid_from_name("root_wfl_pid")
	_root_colloc_pid = get_root_colloc_pid()
	{:ok, file} = File.open "wfl_tree.txt", [:write]
	tree_open = [?{, 34, "wfl_tree", 34,   ?:, ?[]
	tree_close = [?], ?}]
	IO.binwrite file, tree_open
	#types = WFL.get_wfl(root_wfl_pid).types
	wfl_types = WFLScratch.Server.get_sorted_wfl(root_wfl_pid, :freq, :desc) #should only have to sort this once
	Enum.scan(wfl_types, 0, fn({_key, wfl_type}, tally) ->

			if wfl_type.freq > cutoff  && wfl_type.concretisations != nil do
			#this is where we want to create an iolist and output to disk
			#IO.inspect(wfl_type)
				iolist = process_concretisation_type(root_wfl_pid, wfl_type, tally, [])
				#now output the iolist to file
				IO.binwrite file, iolist
				tally + 1
			else
				tally
			end

	end)
	IO.binwrite file, tree_close
	File.close(file)
end

def process_concretisation_type(wfl_pid, %WFL_Type{type: _wfl_type, type_id: type_id, concretisations: concSet}, tally, _iolist) do
	#,{type: "cat", concs: [{type: "cat sat", concs:[]}]}
#IO.inspect({:processing_for, wfl_type})
	#add this token to the iolist pipeline, and recurse over each element in the concretisation map.
	#is it worth seeing if we can revert back to a list of concretisations as opposed to a map?

	conc_list = case concSet do
		nil ->
			[]
		_ ->
			MapSet.to_list(concSet)
	end
#IO.inspect({:conc_list, conc_list})
	xx = X_WFL.expand_type_id(wfl_pid, type_id, true)
	type_op = [?{, 34, "phrase", 34, ?:, 34, "#{xx}", 34, ?,, 34, "concs", 34, ?:, ?[]
	type_open = if tally == 0 do
		type_op
	else
		[?, | type_op]
	end

	type_close = [?], ?}]

	#process_concretisation_type(1, 2, 3)
	#for each element in the concretisations list, create a list of associated wfl_items
		#hopefully we can revert to sing a list instead of a map
		#IO.inspect({:hd, hd(conc_list)})
		{_, io2} = Enum.reduce(conc_list, {0, []}, fn(%Concretisation{pid: conc_pid, token_id: conc_id}, {tally, acc})->
			#IO.inspect(IO.iodata_length(acc))
			conc_info = WFL.get_token_info_from_id(conc_pid, conc_id)
			#IO.inspect(conc_info.type)
			iolist = process_concretisation_type(conc_pid, conc_info, tally, [])
			{tally + 1, [[acc] | iolist]}
		end)	#this will output a new iolist
		[[type_open | io2] | type_close]

end

def get_root_colloc_pid() do
	[_root, root_colloc_pid | _] = X_WFL.get_wfl_chain()
	root_colloc_pid
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
	IO.inspect({:remove, wfl_pid})

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
		IO.inspect({:reject, rejects, :pid, wfl_pid, :dd, dd})

		WFL.update_concretisations(wfl_pid, type_key, Enum.into(filtered_concretisations, MapSet.new()))
	end

	remove_leapfroggers(rest, wfl_pid)

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

def binary8_to_int(bin8) do
	list = for << b::8 <- bin8>>, do: b
	{total, _d} =
			List.foldl(list, {0, 7}, fn(x, {total, depth}) ->
				total2 = total + (round(:math.pow(256,  depth)) * x)
				{total2, depth - 1}
			end)
	total
end


def int_to_binary8(int8) do

	{_x, bin4} =
			List.foldl([0,0,0,0,0,0,0,0], {int8, <<>>}, fn(_x, {remaining, acc}) ->
				divisor = Kernel.trunc(remaining / 256)
				remainder = rem(remaining, 256)
				{divisor, << remainder >> <> acc}
			end)
	bin4
end


def save_sent_offs_io([sent | sents], iolist) do
  #wrapper for sents here

  iolist2 = save_sent_off_io(sent, [?] | iolist], false)
  iolist3 = save_rest_sent_offs_io(sents, iolist2)
	[[?,, 34, "sentences", 34, ?:, ?[] | iolist3]
  #cc = [?]]
  #IO.binwrite f, cc
end

def save_rest_sent_offs_io(sents, iolist) do
	Enum.reduce(sents, iolist, fn(sent, acc) ->
    save_sent_off_io(sent, acc)
  end)
end

def save_sent_off_io({s, {o1, o2}}, iolist, trailing_comma \\true) do

#b = [?,, 34, "sentences", 34, ?:, 32, ?[, ?[, "#{10}", ?,, "#{2}", ?], ?], 125]
#{2, {30, 30}} --> {"s":2, "o":[30,30]}
b = [?[, "#{s}", ?,, "#{o1}", ?,, "#{o2}", ?]]

  bb = case trailing_comma do
    false ->
      b
    _ ->
      [b | [?,]]

  end
  [bb | iolist]
end


def easier_io (wfl_pid) do
	  wfl_types = WFLScratch.Server.get_sorted_wfl(wfl_pid, :freq, :desc)
		# wfl_types2 = Enum.filter(wfl_types, fn({k,v}) ->
		# 	case k do
		# 		"hand" -> true
		# 	#	"the" -> true
		# 	#	"other" -> true
		# 	#	"on" -> true
		# 		_ -> false
		# 	end
		# end)

		new_types = Enum.map(wfl_types, fn ({k, %WFL_Type{freq: freq, type_id: type_id, instances: instances}}) ->
			new_instances = Enum.map(instances, fn({sent_id, {first_off, last_off}}) ->
					[sent_id, first_off,  last_off]
			end)

			t_id = Utils.binary_to_string(type_id)
			%{type: k, type_id: t_id, freq: freq, instances: new_instances}
		end)

		sentenceMap = Sentences.get_map()


		sentenceList = Enum.map(sentenceMap, fn ({sent_id, sent}) ->
			%{sent_id: sent_id, sent: String.reverse(sent)}
		end)
		|> Enum.sort(fn(s1, s2) -> s1.sent_id < s2.sent_id end)

corpus_name = WFLScratch.Server.getCorpusName
		#possible to decorate the struct definitions to include only some fields - however we still need to change token_id and instances
		#if we want to be able to reconstruct the entire wfl then we will have to encode and decode all fields
		xx = %{wfl: %{stats: %{corpus_name: corpus_name, token_count: 1000, type_count: 100}, types: new_types}, sentences: sentenceList}
		{:ok, wfl_json} = Poison.encode(xx)
		IO.inspect(wfl_json)

#poison escapes quote marks so don't send as io list - build string or pass json is as parameter
sql = "insert into public.corpora(corpus_name, wfl_jsonb) VALUES ($1::character varying(20), $2::jsonb)"

#sql = "INSERT INTO public.corpora(corpus_name, wfl_jsonb) VALUES(?, ?)\"" <> corpus_name <> "\", " <> wfl_json <> ")"
#IO.inspect(sql)

		case PostgrexHelper.query(sql, [corpus_name, wfl_json]) do
			:ok ->
				IO.puts("saved #{corpus_name}")
			%Postgrex.Error{} = pge ->
				IO.inspect(pge)
			other ->
			IO.puts("Unexpected event during saving of corpus #{corpus_name}: #{other}")
		end

end

def getCollocs(bin_tokens, offset, sent_id, wfl_pid) do
	throwAwayCount = max(offset - 3, 0)
	{_throwAway, keep} = Enum.split(bin_tokens, throwAwayCount)
	{lhs, [key | rest]} = Enum.split(keep, offset - throwAwayCount)
	rhs =  Enum.slice(rest, 0, 3)

	{key_token, _} = WFL.get_token_from_id(wfl_pid, key)

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
	%{key: key_token, sent_id: sent_id, lhs: lhs_tokens, rhs: rhs_tokens, lhs_sort: lhs_sort}

end

def getCollocations(instances, wfl_pid) do
	# for each instance, get the sentence and extract the collocation
	# at the moment we are just doing this for individual tokens.
	# when we do this  for phrases, we will need to record finish also.

	collocs =
		instances
			|> Enum.map(fn({sent_id, {start, _finish}}) ->
				bin_tokens = TokensBinary.get_bin_tokens(sent_id)
				token_list = for << b::binary-size(4) <- bin_tokens>>, do: b
				getCollocs(token_list, start, sent_id, wfl_pid)
			end)
	#now sort by lhs_rev
	lhs_sort = Enum.sort(collocs, fn(a, b) ->
		a.lhs_sort <=  b.lhs_sort
	end)

	#IO.inspect(lhs_sort)

	{lhs_num_sort, _counter} = List.foldl(lhs_sort, {[], 1}, fn (colloc_map, {collocs, counter}) ->
		{[Map.put(colloc_map, :lhs_sort, counter) | collocs], counter + 1}
	end)

	lhs_num_sort |> Enum.reverse
end

def save_collocations() do
	wfl_pid = X_WFL.get_pid_from_name("root_wfl_pid")
	corpus_name = WFLScratch.Server.getCorpusName
	#poison escapes quote marks so don't send as io list - build string or pass json is as parameter
	sql = "insert into public.collocations(corpus_name, token_id, colloc_jsonb) VALUES ($1::character varying(30), $2::character varying(30), $3::jsonb)"
	types = WFL.get_wfl(wfl_pid).types
	Enum.each(types, fn({key, token_info}) ->
		collocMap = getCollocations(token_info.instances, wfl_pid)
		{:ok, colloc_json} = Poison.encode(collocMap)
			case PostgrexHelper.query(sql, [corpus_name, key, colloc_json]) do
				:ok ->
					IO.puts("saved #{key}")
				%Postgrex.Error{} = pge ->
					IO.inspect(pge)
				other ->
					IO.puts("Unexpected event during saving of collocation #{key}: #{other}")
			end
		end)
end

def save_sentences() do
	corpus_name = WFLScratch.Server.getCorpusName
	#poison escapes quote marks so don't send as io list - build string or pass json is as parameter
	sql = "insert into public.sentences(corpus_name, sentence_jsonb) VALUES ($1::character varying(30), $2::jsonb)"
	sentenceMap = Sentences.get_map()


	sentenceList = Enum.map(sentenceMap, fn ({sent_id, sent}) ->
		%{sent_id: sent_id, sent: String.reverse(sent)}
	end)
	|> Enum.sort(fn(s1, s2) -> s1.sent_id < s2.sent_id end)

	{:ok, sentence_json} = Poison.encode(sentenceList)

	case PostgrexHelper.query(sql, [corpus_name, sentence_json]) do
		:ok ->
			IO.puts("saved sentences")
		%Postgrex.Error{} = pge ->
			IO.inspect(pge)
		other ->
			IO.puts("Unexpected event during saving of sentences #{corpus_name}: #{other}")
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

				%WFL_Type{freq: conc_freq, concretisations: _concs} = WFL.get_token_info_from_id(child_conc_pid, child_conc_id) #we might be able to perform this in one go
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

def get_colloc_stream(wfl) do
	Stream.resource(
		fn ->
			wfl #list of wfl items.
		end,	#this fn intitialises the resource - it takes no params and returns 'the resource' - which will be a sorted wfl
		fn(wfl_item_list) ->
			case wfl_item_list do 	#return next wfl_item.  {:halt, accumulator} when finished.
				[] -> {:halt, []}

		[wfl_item | rest] ->
						{[wfl_item], rest}
			end
		end,
		fn(empty_wfl) -> empty_wfl end 	#this takes accumulator, does clear up and returns the final value if there is one.
	)
end

def filtered_stream(items, filter) do
	r = Stream.resource(
		fn ->
			items #list of wfl items.
		end,	#this fn intitialises the resource - it takes no params and returns 'the resource' - which will be a sorted wfl
		fn(item_list) ->
			case item_list do 	#return next wfl_item.  {:halt, accumulator} when finished.
				[] -> {:halt, []}

				[wfl_item | rest] ->
					{[wfl_item], rest}
			end
		end,
		fn(empty) -> empty end 	#this takes accumulator, does clear up and returns the final value if there is one.
	)
	Stream.filter(r, filter)
end
end
