defmodule WFLScratch.Token do
require Logger

defstruct token: nil, tokens: [], sentence: "", stopper: nil	#each worker should have one of these and Btree shold have its own module / agent

def temp() do
#this is here only to suppress warning about read_token not being used - though it is used via a func ref
cx_read_token("a", {:letter, "", [], [], "", [], 0}, fn x -> x end )
cx_new_token("a", {:letter, "", [], [], "", [], 0}, fn x -> x end )
end


def new() do
#  %WFLScratch.Token{}    

BTree2.new(%{range: 97..122, type: :letter},  &Utils.compare_char_type/2)
	|> BTree2.insert(%{range: 32..32, type: :ws})
	|> BTree2.insert(%{range: 65..90, type: :letter})
	|> BTree2.insert(%{range: 40..44, type: :punct})
	|> BTree2.insert(%{range: 46..46, type: :period})
	|> BTree2.insert(%{range: 48..57, type: :number})
	|> BTree2.insert(%{range: 33..33, type: :stop})
	|> BTree2.insert(%{range: 63..63, type: :stop})
	|> BTree2.insert(%{range: 34..34, type: :quot})
	|> BTree2.insert(%{range: 39..39, type: :apos})
	|> BTree2.insert(%{range: 9..10, type: :ws})
	|> BTree2.insert(%{range: 192..214, type: :letter})
	|> BTree2.insert(%{range: 216..246, type: :letter})
	|> BTree2.insert(%{range: 248..255, type: :letter})
	|> BTree2.insert(%{range: 45..45, type: :hyphen})
	|> BTree2.insert(%{range: 35..38, type: :punct})
	|> BTree2.insert(%{range: 40..44, type: :punct})
	|> BTree2.insert(%{range: 47..47, type: :punct})
	|> BTree2.insert(%{range: 58..62, type: :punct})
	|> BTree2.insert(%{range: 64..64, type: :punct})
	|> BTree2.insert(%{range: 91..96, type: :punct})
	|> BTree2.insert(%{range: 123..126, type: :punct})
	|> BTree2.insert(%{range: 161..172, type: :punct})
	|> BTree2.insert(%{range: 215..215, type: :punct})
	|> BTree2.insert(%{range: 247..247, type: :punct})
	|> BTree2.insert(%{range: 13..13, type: :ws})
	|> BTree2.insert(%{range: 160..160, type: :punct}) #nbs
	|> BTree2.insert(%{range: 8192..8303, type: :punct}) #html5 punct
	

 end

def process_text(filePath) do
	bt = WFLScratch.Token.new()

	#open file - add each token to stack
	File.stream!(filePath) |> Enum.each(fn (line) ->
		process_line(line, bt)	
	end)			
end


def process_line(str, char_tree) when is_binary(str) do
	tokens = defs = sentences = []
	token = sentence = ""
	period_count = 0

	x = _each_char(str, char_tree, {token, 0, tokens, defs, sentence, nil, sentences, period_count}, &WFLScratch.Token.cx_new_token/3)
	|> handleToken()
	|> handleEndline()

	#Logger.debug(x)
	IO.inspect(x)
	
end


def _each_char(<<>>, _char_tree, {_token, _token_count, _tokens, _defs, _sentence, _sentence_id, _sentences, _period_count} = token_data, _handler) do
	#empty string - flush tokenQ
	token_data
end

def _each_char(<<char :: utf8, rest :: binary>>, char_tree, {_token, _token_count, _tokens, _defs, _sentence, _sentence_id, _sentences, _period_count} = token_data, handler) do
	#classify char

	char_def = BTree2.find(char_tree, char)
	
	char_type = char_def[:type]

	if char_type == :unknown do
		Logger.debug("we have an unknown: #{char}")
	end

	
	grapheme = List.to_string([char])	#yuck

	res = handler.(grapheme, char_type, token_data)	#need to pass in more parameters
	
	_each_char(rest, char_tree, res[:token_data],  res[:handler])
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


def handleToken({token, token_count, tokens, defs, sentence, sentence_id, sentences, period_count}) do
	#defs will normally have a whitespace at the end which is carried over to cx_new_token/3 along with any additional punctuation after the token

	toke = get_token(token, defs, period_count)		#if we want to canonicalise tokens we need to return an indicator that this has happened so we don't compare token length with def length.

	toke_len = String.length(toke)
	new_tokens = nil

	if toke_len > 0 do
		new_tokens = [{toke, token_count} | tokens]
		token_count = token_count + 1				
	end

	post_def_len = length(defs) - toke_len

	post_defs = Enum.take(defs, post_def_len)
	post_chars = String.slice(sentence, 0, post_def_len)

	[token_data: {post_chars, token_count, new_tokens||tokens, post_defs, sentence, sentence_id, sentences, period_count}, handler: &WFLScratch.Token.cx_new_token/3]

end

def handleSentence({token, next_char_type, tokens, defs, sentence, sentence_id, sentences, punct_len}, handler) do
#defs does not include char_type of next char
#output looks strange.  we need to clarify what inputs are and what we should be doing with them.
	trim_len = length(defs) - punct_len
	post_sent_defs = Enum.take_while(defs, fn x -> x != :ws end)
	len_post_defs = length(post_sent_defs)

	sent_start = if len_post_defs > 0 do 
		String.slice(sentence, 0..length(post_sent_defs) - 1)
	else
		""
	end

	new_sent = trimSent(sentence, trim_len)

	[token_data: {token, 0, tokens, [next_char_type], <<token <> sent_start>>, sentence_id, [{new_sent, sentence_id}|sentences], 0}, handler: handler]

end

def handleEndline([token_data: token_data,  handler: handler]) do
	{token, _token_count, tokens, defs, sentence, sentence_id, sentences, punct_len} = token_data
	handleSentence({token, :end, tokens, defs, sentence, sentence_id, sentences, punct_len}, handler)
end

def cx_new_token(char, char_type, {token, token_count, tokens, defs, sentence, sentence_id, sentences, _period_count} ) do
	#looking for an alpha numeric to start a new token - also for sentence boundary
	
	case char_type do
		y when y == :letter or y == :number ->  #use binary attributes?
	
		#we have the start of a new token (in a new sentence)
		s_id = SentenceCounter.get_sentence_id(:sent_id_gen)
		
		# check if we have crossed a sentence boundary in the process
		rev_defs = Enum.reverse(defs)


		{result, punct_len} = isSentence?(rev_defs, token)

		if result == true do			
			handleSentence({char, char_type, tokens, defs, sentence, s_id, sentences, punct_len}, &WFLScratch.Token.cx_read_token/3) 
		else
			[token_data: {char, token_count, tokens, [char_type], <<char <> sentence>>, s_id, sentences, 0}, handler: &WFLScratch.Token.cx_read_token/3]
		end

		#add char to token to and char info to chardefs

	_ ->
		#add token info to idefs, and continue to look for token start
		[token_data: {token, token_count, tokens, [char_type|defs], <<char <> sentence>>, sentence_id, sentences, 0}, handler: &WFLScratch.Token.cx_new_token/3]
	end
	
end


def cx_read_token(char, char_type, {token, token_count, tokens, defs, sentence, sentence_id, sentences, period_count} ) do
	#we are collecting characters for a token - keep going till we get whitespace, count any periods on the way

	new_sentence = <<char <> sentence>>
	new_defs = [char_type|defs]

	case char_type do
		:ws ->
			handleToken({token, token_count, tokens, new_defs, new_sentence, sentence_id, sentences, period_count}) 	#new_defs has the extra whitespace while token does not

		:period ->
			[token_data: {<<char <> token>>, token_count, tokens, new_defs, new_sentence, sentence_id, sentences, period_count + 1}, handler: &WFLScratch.Token.cx_read_token/3]	

		_ ->
			#add this character to the token queue
		
			[token_data: {<<char <> token>>, token_count, tokens, new_defs, new_sentence, sentence_id, sentences, period_count}, handler: &WFLScratch.Token.cx_read_token/3]		
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


def handle_sent(token, next_char_type, tokens, defs, sentence, sentence_id, sentences, period_pos, handler) do
	handleSentence({token, next_char_type, tokens, defs, sentence, sentence_id, sentences, period_pos}, handler) 
end


def trim_sent(sent, num) do
	trimSent(sent, num)
end

def test_log(str) do
	Logger.info(str)
end

defp list_items_to_string(a_list) do
	Enum.map(a_list, fn x -> to_string(x) end)
end

end