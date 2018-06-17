# #wfl_types.ex
# 	defmodule WFL_Type do
# 		defstruct([:type, :type_id, :freq, is_common: false, instances: [], concretisations: []])	#concretisations holds token_ids of types that extend the current type ie catsat extends cat and sat
# 	end
#
# 	defmodule WFL_Data do
# 		defstruct([depth: 0, types: %{}, type_ids: %{}])	#both types and type_ids map into the same WFL_Type collections
# 	end
#
# defmodule SentenceInfo do
# 		#this struct is used when reading text file initially
# 		defstruct([tokens: [], sentence: <<>>])
# 	end
#
# 	defmodule TokenInfo do
# 		defstruct([token: "" , token_count: 0, char_type: :none, defs: [], period_count: 0, punct_len: 0])
# 	end
#
# defmodule ReaderInfo do
# 		defstruct([token_info: %TokenInfo{}, sentence_info: %SentenceInfo{}, sentences: []])  #sentences is an array of %SentenceInfo
# 	end
# #end wfl_types.ex

defmodule TextReader do
	require Logger

	defstruct([reader_info: %ReaderInfo{}, handler: &TextReader.cx_new_token/3])

	#API
	def processText(filePath, char_def_tree, server, wfl_pid) do
		#spawn a process that will start reader and listen out for completion
		spawn(fn -> process_file(filePath, wfl_pid, char_def_tree, server) end)	#this function is being called on client thread so don't spawn link here
		:ok
	end


	defp process_file(filePath, wfl_pid, char_def_tree, server) do
		#start monitored process to read file
		_res = spawn_monitor(fn ->read_file(filePath, wfl_pid, char_def_tree) end)

		#now wait for the processing to finish and alert wfl_server
		#IO.puts inspect res
		receive do
			{:DOWN, _ref, :process, _pid, :normal} ->
				IO.puts "File complete: Normal"
				send(server, {:file_complete, wfl_pid})
			{_DOWN, _ref, _process, _pid, {{:nocatch, _}, _}} ->
				IO.puts "File read: Unhandled exception"
				send(server, {:file_error, filePath})
			msg ->
				IO.puts "File read: Something else"
				IO.inspect(msg)
				send(server, {:file_complete, filePath})
		end
	end

	defp read_file(filePath, wfl_pid, char_def_tree) do
		#open file - add each token to stack
		#for each line is the reader, add token to the wfl
		#each iteration of reader should supply a list of tokens that comprise a sentence ["the", "cat", "sat", "on", "the", "mat"]
		#we may want original file name or reference to it and offset in that file - but deal with that as needs be.
		#open file - add each token to stack
		File.stream!(filePath) |> Enum.each(fn (line) ->
			process_line(line, wfl_pid, char_def_tree)
		end)

	end


def process_line(str, wfl_pid, char_tree) when is_binary(str) do

	%ReaderInfo{sentences: sentences} = for_each_char(str, char_tree, %TextReader{})
		|> handleToken()
		|> handleEndline()	#handleToken and handleEndline are only called once after line has been processed to mop up last word/sentence

	#IO.inspect(sentences)

	#sample_sentenceInfo = [%SentenceInfo{sentence: "yad ykcul ruoy eb thgim tI",
  										#tokens: ["day", "lucky", "your", "be", "might", "it"]}]
	#note that the full stop is missing here, perhaps because it coincides with an endline.

	GenServer.cast(wfl_pid, {:add_tokens, sentences})	#this should accept spawn acknowledge
end

def process_line(_str, _wfl_pid, _char_tree) do
end

def for_each_char(<<>>, _char_tree, %TextReader{reader_info: %ReaderInfo{} = reader_info}) do
	#finished processing line
	reader_info
end


def for_each_char(<<char :: utf8, rest :: binary>>, char_tree, %TextReader{reader_info: %ReaderInfo{} = reader_info, handler: handler}) do
	#classify char

	char_def = BTree2.find(char_tree, char)
	char_type = char_def[:type]

	if char_type == :unknown do
		Logger.debug("we have an unknown: #{char}")
	end

	grapheme = List.to_string([char])	#yuck
	new_text_reader = handler.(grapheme, char_type, reader_info)
	#new_text_reader = %ReaderInfo{reader_info: %ReaderInfo{reader_info | token_info: | token_data: res.token_data}
	for_each_char(rest, char_tree, new_text_reader)
end


def trimSent(sent, num) when num > 0 do
	s = String.slice(sent, 0, num)
	bs = byte_size(s)	#bs will normally be num and it is probably fairly safe to use num as two byte chars are normally letters.
	<<_ :: binary-size(bs), rest :: binary>> = sent
	rest
end


def trimSent(sent, _num) do
	sent
end


def handleToken(%ReaderInfo{sent_index: sent_index, token_info: %TokenInfo{token: token, token_count: token_count, offset: offset, defs: defs, period_count: period_count}= token_info,  sentence_info: %SentenceInfo{tokens: tokens, sentence: sentence}, sentences: sentence_infos}) do
		#defs will normally have a whitespace at the end which is carried over to cx_new_token/3 along with any additional punctuation after the token

	toke = get_token(token, defs, period_count)		#if we want to canonicalise tokens we need to return an indicator that this has happened so we don't compare token length with def length.
	toke_len = String.length(toke)

	{new_tokens, new_token_count} =
		if toke_len > 0 do
			{[%{token: toke, offset: offset} | tokens], token_count + 1}
		else
			{nil, token_count}
		end

	post_def_len = length(defs) - toke_len

	post_defs = Enum.take(defs, post_def_len)
	post_chars = String.slice(sentence, 0, post_def_len)
	new_token_info = %TokenInfo{token_info | token: post_chars, token_count: new_token_count, defs: post_defs}
	new_sentence_info = %SentenceInfo{tokens: new_tokens||tokens, sentence: sentence}

	%TextReader{reader_info: %ReaderInfo{sent_index: sent_index, token_info: new_token_info, sentence_info: new_sentence_info, sentences: sentence_infos}, handler: &TextReader.cx_new_token/3} 	#new_defs has the extra whitespace while token does not

end

def handleSentence(%ReaderInfo{sent_index: sent_index, token_info: %TokenInfo{token: token, defs: [next_char_type | defs], punct_len: punct_len}, sentence_info: %SentenceInfo{sentence: sentence, tokens: tokens} = sentence_info, sentences: sentence_infos}) do
	#need to check if sentence long enough - has any content
	trim_len = length(defs) - punct_len
	post_sent_defs = Enum.take_while(defs, fn x -> x != :ws end)
	len_post_defs = length(post_sent_defs)

	sent_start = if len_post_defs > 0 do
		String.slice(sentence, 0..length(post_sent_defs) - 1)
	else
		""
	end

	new_sent = trimSent(sentence, trim_len)	#this is trimmed sentence - we either need access to the wfl now.. or we store back into sentence
	#Logger.debug("new sent: #{new_sent}")

	#should do a merge with existing reader info object - here we repeat period_count and punct_len default values.

	%ReaderInfo{
		sent_index: sent_index,
		token_info: %TokenInfo{token: token, defs: [next_char_type]},
		sentence_info: %SentenceInfo{sentence_info | tokens: [], sentence: <<token <> sent_start>>},
		sentences: [%SentenceInfo{tokens: tokens, sentence: new_sent}  | sentence_infos]
	}

end

def handleEndline(%TextReader{reader_info: %ReaderInfo{token_info: %TokenInfo{defs: defs} = token_info} = reader_info}) do
	#{token, _token_count, tokens, defs, sentence, sentences, punct_len} = token_data
	##handleSentence(%ReaderInfo{token_info: %TokenInfo{token_info |  token: char, defs: [char_type | defs], punct_len: punct_len}, sentence_info: sentence_info, sentences: sentence_infos})

{_result, punct_len} =
	Enum.reverse(defs)
	|> isSentence?("")
#IO.inspect(isSentence?(, ""))
	case token_info.token_count do
		token_count when token_count > 0 ->
			new_token_info = %TokenInfo{token_info | defs: [:end | defs], punct_len: punct_len}
			new_reader_info = %ReaderInfo{reader_info | token_info: new_token_info}
			handleSentence(new_reader_info)

		_ -> %ReaderInfo{}
	end

end

def cx_new_token(char, char_type, %ReaderInfo{sent_index: sent_index, token_info: %TokenInfo{token: token, offset: offset, defs: defs} = token_info, sentence_info: %SentenceInfo{sentence: sentence} = sentence_info, sentences: sentence_infos }) do
	#IO.inspect(zz)
	#looking for an alpha numeric to start a new token - also for sentence boundary

	case char_type do
		y when y == :letter or y == :number ->  #use binary attributes?

		# check if we have crossed a sentence boundary in the process
		rev_defs = Enum.reverse(defs)

		{result, punct_len} = isSentence?(rev_defs, token)

		if result == true do
			#we have the start of a new token (in a new sentence)
			#s_id = SentenceCounter.get_sentence_id(:sent_id_gen)
			new_reader_info = handleSentence(%ReaderInfo{sent_index: 1, token_info: %TokenInfo{token_info | token: char, offset: 0, defs: [char_type | defs], punct_len: punct_len}, sentence_info: sentence_info, sentences: sentence_infos})
			%TextReader{reader_info: new_reader_info,  handler: &TextReader.cx_read_token/3}
		else
			new_reader_info = %ReaderInfo{sent_index: sent_index + 1, token_info: %TokenInfo{token_info | token: char, offset: sent_index, defs: [char_type], period_count: 0}, sentence_info: %SentenceInfo{sentence_info | sentence: <<char <> sentence>>}, sentences: sentence_infos}
			#IO.inspect(new_reader_info)
			%TextReader{reader_info: new_reader_info,  handler: &TextReader.cx_read_token/3}
		end

	_ ->
		new_sent = if token_info.token_count > 0 do
			<<char <> sentence>>
		else
			sentence
		end

		#IO.inspect(new_sent)

		#add token info to idefs, and continue to look for token start
		new_reader_info = %ReaderInfo{sent_index: sent_index + 1, token_info: %TokenInfo{token_info | defs: [char_type|defs]}, sentence_info: %SentenceInfo{sentence_info | sentence: new_sent}, sentences: sentence_infos}
		%TextReader{reader_info: new_reader_info, handler: &TextReader.cx_new_token/3}

	end
end


def cx_read_token(char, char_type, %ReaderInfo{sent_index: sent_index, token_info: %TokenInfo{} = token_info, sentence_info: %SentenceInfo{} = sentence_info, sentences: sentence_infos}) do
	#we are collecting characters for a token - keep going till we get whitespace, count any periods on the way
#IO.inspect (sentence_info)
	new_sentence = <<char <> sentence_info.sentence>>
	new_defs = [char_type|token_info.defs]

	case char_type do
		:ws ->
			new_reader_info = %ReaderInfo{sent_index: sent_index + 1, token_info: %TokenInfo{token_info | defs: new_defs}, sentence_info: %SentenceInfo{sentence_info | sentence: new_sentence}, sentences: sentence_infos}
			handleToken(new_reader_info) 	#new_defs has the extra whitespace while token does not
			#why does this not return a char reading function?

		:period ->
			new_reader_info = %ReaderInfo{sent_index: sent_index + 1, token_info: %TokenInfo{token_info | token: <<char <> token_info.token>>, defs: new_defs, period_count: token_info.period_count + 1}, sentence_info: %SentenceInfo{sentence_info | sentence: new_sentence}, sentences: sentence_infos}
			%TextReader{reader_info: new_reader_info, handler: &TextReader.cx_read_token/3}

		_ ->
			#add this character to the token queue
			new_reader_info = %ReaderInfo{sent_index: sent_index + 1, token_info: %TokenInfo{token_info | token: <<char <> token_info.token>>, defs: new_defs}, sentence_info: %SentenceInfo{sentence_info | sentence: new_sentence}, sentences: sentence_infos}
			%TextReader{reader_info: new_reader_info, handler: &TextReader.cx_read_token/3}
	end
end


def checkSent([:ws | [:ws | _rest]], _token) do
	#period 2 white space
	true
end

def checkSent([:ws | _rest], token) do
	#period 1 space if not abbrev
	if isKnownAbbrev?(token) do
		false
	else
		#count this as a sentence boundary
		true
	end
end

def checkSent([:punct | _rest], _token) do
	#period followed by punctuation - count as sentence boundary
	true
end

def checkSent(_defs, _token) do
	false
end


def extra_punct(defs) do
	Enum.take_while(defs, fn x -> x != :ws end)
end

def isSentence?(idefs, token) do
	#hook for testing - otherwise redundant
	_isSentence?(idefs, token)
end

def _isSentence?([], _token) do
	{false, 0}
end

def _isSentence?([:period | idefs], token) do
	#returns number of chars to include with sentence

	if checkSent(idefs, token) == true do
		punct_defs = extra_punct(idefs)
		{true, length(punct_defs) + 1}
	else
		_isSentence?(idefs, token)
	end
end


def _isSentence?([:stop | idefs], _token) do
	punct_defs = extra_punct(idefs)
	{true, length(punct_defs) + 1}
end

def _isSentence?([_idef | idefs], token) do
	_isSentence?(idefs, token)
end

def isKnownAbbrev?(_token) do
	false	#stub out for now
end


def get_token(token, defs, pc) do
	#can we search for sentence boundary at the same time?
	toke_defs = extract_token(defs, pc)
	toke_len = length(toke_defs)
	rev_str = String.reverse(String.downcase(token))		#consider not reversing string for all tokens - just do it for the types.
	toke = String.slice(rev_str, 0, toke_len)

	_x = #this is here for when we introduce canonicalisation of numbers - perhaps we should handle this higher up.
	case toke_defs do
		[:number | _rest] ->
			case Float.parse(toke) do
				{:error, _} ->
					toke
				{_, ""} ->
					"#num#"
				_ ->
					toke
			end
		_ ->
			toke
	end
	toke
end

def extract_token([:letter | _defs] = char_defs, _pc)  do
	#this is the word boundary
	char_defs
end

def extract_token([:period | [:letter | _]] = char_defs, pc)  when pc > 1 do

	#alpha followed by period and there is more than one period so assume token is abbreviation such as a.m.
	char_defs

end


def extract_token([:period | defs], pc) do
	#assume the period is not part of the token until we reach alpha-numeric.
	extract_token(defs, pc - 1)

end


def extract_token([:number | _defs] = char_defs, _pc)  do
	#this is the word boundary
	char_defs
end


def extract_token([_char_type | defs], pc) do
	#we have something that is not a letter, number, period  or white-space - so ignore
	extract_token(defs, pc)
end


def extract_token([], _pc) do
	#no numbers or letters encountered (presumably at line end)
	[]
end


def reverse(l) do
	List.foldl(l, [], fn (x, acc) -> [x | acc] end)
end


#test
def get_tok(token, defs, pc) do
	get_token(token, defs, pc)
end

def handle_tok({token, tokens, defs, sentence, sentences, period_count}) do
	handleToken({token, tokens, defs, sentence, sentences, period_count})
end

def trim_sent(sent, num) do
	trimSent(sent, num)
end

def test_log(str) do
	Logger.info(str)
end

def list_items_to_string(a_list) do
	Enum.map(a_list, fn x -> to_string(x) end)
end

end
