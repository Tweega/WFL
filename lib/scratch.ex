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


def test_stream(wfl_pid) do
	file = File.stream!("wfl.txt")

	#s = WFLStream.get_wfl_stream(wfl_pid)
	#Stream.map(WFL.get_wfl(wfl_pid).types, fn (k) ->
	#after all this faffing around with streams - I actually want to sort the wfl by freq.
	#Stream.map(WFL.get_wfl(wfl_pid).types, fn ({k, v}) ->

  wfl_types = WFLScratch.Server.get_sorted_wfl(wfl_pid, :freq, :asc)

  Stream.transform(wfl_types, [], fn (v, acc) ->
	IO.inspect(v)
  case v do
    [] -> {:halt, acc}
  end
		#{}"{" <> k <> ":" <> "\"" v.freq <> "}"
		# "{#{k}: #{v.freq}}"
    {tl(v), acc}
	end)
  # i don't think that we want stream.transform.
  # we want to forward a single wfl_type
  # may want to forget streaming just for the moment.
	#File.close(file)

#   stream = File.stream!("code")
# |> Stream.map(&String.replace(&1, "#", "%"))
# |> Stream.into(File.stream!("new"))
# |> Stream.run

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

	iolist = [?}]

	iolist2 = save_sents(v.instances, iolist)
	iolist3 =
		if v.is_common == true do
			[?,, 32, 34, "common", 34, ?:, "#{1}" | iolist2]
		else
			iolist2
		end

  a = [123, 34, "type", 34, ?:, 34, k, 34, ?,, 34, "freq", 34, ?:, "#{v.freq}", 10, 13]

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

def save_sents([sent | sents], iolist) do
  #wrapper for sents here

  bb = [?,, 34, "sentences", 34, ?:, ?[]
  ##IO.binwrite f, bb
  iolist2 = save_sent(sent, [?] | iolist], false)
  iolist3 = save_rest_sents(sents, iolist2)
	[[?,, 34, "sentences", 34, ?:, ?[] | iolist3]
  #cc = [?]]
  #IO.binwrite f, cc
end

def save_rest_sents(sents, iolist) do
	Enum.reduce(sents, iolist, fn(sent, acc) ->
    save_sent(sent, acc)
  end)
end

def save_sent({s, {o1, o2}}, iolist, leading_comma \\true) do

#b = [?,, 34, "sentences", 34, ?:, 32, ?[, ?[, "#{10}", ?,, "#{2}", ?], ?], 125]
IO.inspect(s)
IO.inspect(o1)
IO.inspect(o2)
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

def save_wfl(wfl_pid) do

  {:ok, file} = File.open "wfl.txt", [:write]
IO.binwrite file, [?{, 34, "wfl", 34,   ?:, ?[]
  types = WFL.get_wfl(wfl_pid).types
  wfl_types = WFLScratch.Server.get_sorted_wfl(wfl_pid, :freq, :asc)
#IO.inspect(types)
IO.puts("Bonjour")
save_tokens(wfl_types, file)

  IO.binwrite file, [?], ?}]

  File.close(file)
end

end
