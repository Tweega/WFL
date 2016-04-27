defmodule SentenceData do	
	defstruct([token: "" , token_count: 0, tokens: [], defs: [], sentence: <<>>, sentences: [], period_count: 0, punct_len: 0, char_type: :none ])
end

defmodule TextReader do
	require Logger

	@name :wfl_server
	use GenServer

	defstruct([sentence_data: %SentenceData{}, handler: &TextReader.cx_new_token/3])
	
	#API
	def processText(filePath) do
		#may want to process text with specific helper.
		:gen_server.cast(@name, {:wfl_file, filePath })
	end

	def start_link(state) do		
		#need to raise error if no wfl_pid - might even check if something on the other end
		#might be passed in a helper to read text
		#we either need to register the name of the sever or keep it in state
		:gen_server.start_link(__MODULE__, state, [])
	end


	#Server
	def init(wfl_pid, char_def_tree) do
		#{:ok, stoppers} = WFLScratch.Stoppers.start_link()	# we will need a superviser here and rather than create the process here we should be either be passed the name of the process
		#should this be part of a helper?
		
		{:ok, {wfl_pid, char_def_tree}}	#use a struct?
	end

	def handle_cast( {:wfl_file, filePath}, {wfl_pid, bt} = state) do
		_s = process_wfl(filePath, wfl_pid, bt)
		{:noreply, state}
	end

	defp process_wfl(filePath, wfl_pid, char_def_tree) do
		#open file - add each token to stack
		#for each line is the reader, add token to the wfl
		#each iteration of reader should supply a list of tokens that comprise a sentence ["the", "cat", "sat", "on", "the", "mat"]
		#we may want original file name or reference to it and offset in that file - but deal with that as needs be.
		#open file - add each token to stack
		File.stream!(filePath) |> Enum.each(fn (line) ->
			process_line(line, wfl_pid, char_def_tree)
		end)
		
	end


def process_line(str, _wfl_pid, char_tree) when is_binary(str) do
	
	tokes = _each_char(str, char_tree, %TextReader{handler: &TextReader.cx_new_token/3})
		|> handleToken()
	
	sentences = handleEndline(tokes).sentences

	Enum.each(sentences, fn s -> 
		Logger.debug(s)
	end)
	###|> WFL.addTokens(wfl_pid

	#Logger.debug(tokes)
	IO.inspect(tokes)
	
end


def _each_char(<<>>, _char_tree, %TextReader{sentence_data: %SentenceData{} = sentence_data}) do
	#empty string - flush tokenQ
	sentence_data
end

def _each_char(<<char :: utf8, rest :: binary>>, char_tree, %TextReader{sentence_data: %SentenceData{} = sentence_data}, handler: handler) do
	#classify char

	char_def = BTree2.find(char_tree, char)
	
	char_type = char_def[:type]

	if char_type == :unknown do
		Logger.debug("we have an unknown: #{char}")
	end

	
	grapheme = List.to_string([char])	#yuck

	res = handler.(grapheme, char_type, sentence_data)	#need to pass in more parameters
	
	_each_char(rest, char_tree, %TextReader{sentence_data: res[:sentence_data],  handler: res[:handler]})
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


def handleToken(%SentenceData{token: token, token_count: token_count, tokens: tokens, defs: defs, sentence: sentence, sentences: sentences, period_count: period_count}) do
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

	%TextReader{sentence_data: %SentenceData{token: post_chars, token_count: token_count, tokens: new_tokens||tokens, defs: post_defs, sentence: sentence, sentences: sentences, period_count: period_count}, handler: &TextReader.cx_new_token/3} 	#new_defs has the extra whitespace while token does not

end

def handleSentence(%TextReader{sentence_data: %SentenceData{token: token, defs: defs, sentence: sentence, sentences: sentences, period_count: punct_len}, handler: handler}) do
#output looks strange.  we need to clarify what inputs are and what we should be doing with them.
#need to check if sentence long enough - has any content
	trim_len = length(defs) - punct_len
	post_sent_defs = Enum.take_while(defs, fn x -> x != :ws end)
	len_post_defs = length(post_sent_defs)

	sent_start = if len_post_defs > 0 do 
		String.slice(sentence, 0..length(post_sent_defs) - 1)
	else
		""
	end

	new_sent = trimSent(sentence, trim_len)
	logger.debug("new sent: #{new_sent}")
	sd = %SentenceData{sentence_data | sentence: <<token <> sent_start>>, sentences: [new_sent|sentences], period_count: 0}
	#note that handleSentence does not need to return anything as nothing is carried over an endline boundary.
	%TextReader{sentence_data: sd, handler: handler}

end

def handleEndline(%TextReader{sentence_data: sentence_data, handler: handler}) do
	#{token, _token_count, tokens, defs, sentence, sentences, punct_len} = sentence_data
	sd = %SentenceData{sentence_data | }
	#note that handleEndline does not need to return anything as nothing is carried over an endline boundary.
	handleSentence({token, :end, tokens, defs, sentence, sentences, punct_len}, handler)
end

def cx_new_token(char, char_type, %SentenceData{token: token, sentence: sentence} = sentence_data ) do
	#looking for an alpha numeric to start a new token - also for sentence boundary
	
	case char_type do
		y when y == :letter or y == :number ->  #use binary attributes?
	
		# check if we have crossed a sentence boundary in the process
		rev_defs = Enum.reverse(defs)


		{result, punct_len} = isSentence?(rev_defs, token)

		if result == true do			
			#we have the start of a new token (in a new sentence)
			#s_id = SentenceCounter.get_sentence_id(:sent_id_gen)					
			#check that handleSentence returns a text reader
			sd = handleSentence(%SentenceData{sentence_data |  punct_len: punct_len}, &TextReader.cx_read_token/3) 
			%TextReader{sentence_data: sd}
		else
			sd = %SentenceData{sentence_data | defs: [char_type], sentence: <<char <> sentence>>} 
			%TextReader{sentence_data: sd, handler: &TextReader.cx_read_token/3}			
		end

		#add char to token to and char info to chardefs

	_ ->
		#add token info to idefs, and continue to look for token start		
		sd = %SentenceData{sentence_data | defs: [char_type|defs], sentence: <<char <> sentence>>} 
		%TextReader{sentence_data: sd, handler: &TextReader.cx_new_token/3}			
	end	
end


def cx_read_token(char, char_type, %SentenceData{} = sentence_data ) do
	#we are collecting characters for a token - keep going till we get whitespace, count any periods on the way

	new_sentence = <<char <> sentence_data.sentence>>
	new_defs = [char_type|sentence_data.defs]

	case char_type do
		:ws ->
			sd = %SentenceData{sentence_data | defs: new_defs, sentence: new_sentence}
			handleToken(sd) 	#new_defs has the extra whitespace while token does not
			#why does this not return a char reading function?

		:period ->
			sd = %SentenceData{sentence_data | token: <<char <> sentence_data.token>>, sentence: new_sentence, defs: new_defs, period_count: period_count + 1}
			%TextReader{sentence_data: sd, handler: &TextReader.cx_read_token/3}	

		_ ->
			#add this character to the token queue
			sd = %SentenceData{sentence_data | token: <<char <> sentence_data.token>>, defs: new_defs, sentence: new_sentence}
			%TextReader{sentence_data: sd, handler: &TextReader.cx_read_token/3}		
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


def handle_sent(token, next_char_type, tokens, defs, sentence, sentences, period_pos, handler) do
	handleSentence({token, next_char_type, tokens, defs, sentence, sentences, period_pos}, handler) 
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
