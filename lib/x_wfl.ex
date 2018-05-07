defmodule X_WFL do
	use GenServer
	#API

	# def translate_phrase(wfl_pid, phrase) do
  #   #this might belong here by virtue of only applying to the root wfl?
	# 	:gen_server.call(wfl_pid, {:translate_phrase, phrase})
	# end

  def expand_type_id(wfl_pid, type_id, to_text \\ true) do
		:gen_server.call(:xwfl, {:expand_type_id, wfl_pid, type_id, to_text})
	end

	def expand_type_id_to_string(wfl_pid, type_id) do
		:gen_server.call(:xwfl, {:expand_type_id_to_string, wfl_pid, type_id})
	end

	def expand_wfl(wfl_pid, to_text \\ true) do
		:gen_server.call(:xwfl, {:expand_wfl, wfl_pid, to_text})
	end

	def get_wfl_chain() do
		:gen_server.call(:xwfl, {:get_wfl_chain})
	end

	def get_pid_from_name(pid_name) do
		:gen_server.call(:xwfl, {:get_pid_from_name, pid_name})
	end

	def start_link(first_last_pid) do
    GenServer.start_link(__MODULE__, first_last_pid, name: :xwfl)
		#:gen_server.start_link(__MODULE__, [root_wfl_pid], :name :XWFL)
	end

	#Server
	def init(state) do
    #IO.inspect({:xwfl_state, state})
		{:ok, state}
	end


	def handle_call({:expand_wfl, wfl_pid, to_text}, _from, {root_wfl_pid, _last_wfl_pid} = pids) do

		#add a check to see if wfl_pid is the root wfl pid, in which case we just want a dump of the wfl - or at least the types.
		{wfl, parent_pid} = WFL.get_wfl_state(wfl_pid)
		#grandparent_pid = WFL.get_parent(parent_pid)
		Enum.each(wfl.type_ids, fn({token_id, _phrase})  ->
			tok = exp_token(token_id, wfl_pid, parent_pid, root_wfl_pid, to_text)
			f = WFL.get_token_info_from_id(wfl_pid, token_id).freq
			IO.inspect({tok, f})
		end)

		{:reply, :ok, pids}
	end


  def handle_call({:expand_type_id, wfl_pid, token_id, to_text}, _from, {root_wfl_pid, _last_wfl_pid} = pids) do
    parent_wfl_pid = WFL.get_parent(wfl_pid)

		tok = exp_token(token_id, wfl_pid, parent_wfl_pid, root_wfl_pid, to_text)
    #IO.inspect({:exp_tok, tok})
		{:reply, tok, pids}
	end

	def handle_call({:expand_type_id_to_string, wfl_pid, token_id}, _from, {root_wfl_pid, _last_wfl_pid} = pids) do
    parent_wfl_pid = WFL.get_parent(wfl_pid)

		tok = exp_token(token_id, wfl_pid, parent_wfl_pid, root_wfl_pid, true)
		phrase = Enum.join(tok, " ")
    #IO.inspect({:exp_tok, tok})
		{:reply, phrase, pids}
	end


	def handle_call({:get_wfl_chain}, _from, {_root_wfl_pid, last_wfl_pid} = pids) do  #should also store last_wfl_pid here
		wfl_chain = get_chain(last_wfl_pid)
		{:reply, wfl_chain, pids}
	end


	def handle_call({:get_pid_from_name, pid_name}, _from, {root_wfl_pid, last_wfl_pid} = pids) do
		pid = case pid_name do
			"last_wfl_pid" ->
				last_wfl_pid
			_ ->
			root_wfl_pid
		end
		{:reply, pid, pids}
	end


  def exp_token(token_id, wfl_pid, parent_wfl_pid, root_wfl_pid, to_text) do
		phrase = xp_token([{token_id, wfl_pid, parent_wfl_pid}], root_wfl_pid, <<>>)
		if to_text == true do
			WFL.translate_phrase(root_wfl_pid, phrase)
		else
			Utils.rev_bin4(phrase)
		end
	end

	def xp_token([], _root_wfl_pid, phrase) do
		#we've finished if there are no more tokens to process for this chunk, and there are no remaining chunks left
		phrase
	end

	def xp_token([{token_id, _wfl_pid, parent_wfl_pid} | rest], root_wfl_pid, phrase) when is_nil(parent_wfl_pid) do
		#this token_id is not expandable (points to real word)
		new_phrase = token_id <> phrase
		xp_token(rest, root_wfl_pid, new_phrase)
	end

	def xp_token([{token_id, wfl_pid, parent_wfl_pid} | rest_tokens], root_wfl_pid, phrase) do
    #any call to wfl_pid will be calling into the same pid.  so there is a question of whether wfl_pid
    #should be passed in.
		#this token_id is expandable
		#mind the gap
		<<_gap_byte :: binary-size(1),  rest_bytes :: binary-size(3)>> = token_id
		token_id_without_gap = <<0x00 :: integer-unit(8)-size(1)>> <> rest_bytes
# IO.inspect({:self, self()})
# IO.inspect({:calling, wfl_pid})

#defp fetch_token_from_id({%WFL_Data{} = wfl_data, parent_wfl_pid}, token_id) do
#so we need to be able to call get_token_from_id without calling the API.  need to call it directly.

		{token_type, _lhs_parent_wfl_pid} = WFL.get_token_from_id(wfl_pid, token_id_without_gap)
    #IO.inspect("comment allez vous")
		grandparent_wfl_pid = WFL.get_parent(parent_wfl_pid) # if this is a problem then we will need the colloc chain and parentage in named wfl
		<<lhs :: binary-size(4), rhs :: binary-size(4)>> = token_type

		new_stack = [{lhs, parent_wfl_pid, grandparent_wfl_pid} | [{rhs, root_wfl_pid, nil} | rest_tokens]]
		temp = xp_token(new_stack, root_wfl_pid, phrase)
    #IO.inspect("exiting")
    temp
	end

	def get_chain(wfl_pid) do
		parent_wfl_pid = WFL.get_parent(wfl_pid)
		get_chain(wfl_pid, parent_wfl_pid, [])
	end

	defp get_chain(wfl_pid, parent_wfl_pid, acc) when is_nil(parent_wfl_pid)  do
		[wfl_pid | acc]
	end

	defp get_chain(wfl_pid, parent_wfl_pid, acc) do
		grandparent_wfl_pid = WFL.get_parent(parent_wfl_pid)
		new_acc = [wfl_pid | acc]
		get_chain(parent_wfl_pid, grandparent_wfl_pid, new_acc)
	end

end
