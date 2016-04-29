defmodule WFLScratch.Server do
	use GenServer

	
	#API
	def processWFL(filePath) do
		:gen_server.cast(:WFL, {:wfl_file, filePath })
	end

	def start_link(stack) do
		:gen_server.start_link({:local, :WFL}, __MODULE__, stack, [])
	end


	#Server
	def init(stack) do
		#{:ok, stoppers} = WFLScratch.Stoppers.start_link()	# we will need a superviser here and rather than create the process here we should be either be passed the name of the process
		{:ok, stack}
	end

	def handle_call(:pop, _from, [h|stack]) do
		{:reply, h, stack}
	end

	def handle_cast( {:push, new}, stack) do
		{:noreply, [new|stack]}
	end
	
	def handle_cast( {:wfl_file, filePath}, stack) do
		_s = process_wfl(filePath)
		{:noreply, stack}
	end

	defp process_wfl(filePath) do
		#open file - add each token to stack
		File.stream!(filePath)  |> Enum.each(fn (line) ->
  			#:gen_server.cast(:WFL, {:push, line})
  			tokenise(String.codepoints(line))  			
		end)			
	end

	defp tokenise([]) do

	end

	defp tokenise([h|t]) do
		#check if h is a stop char - for this we might want to consult an agent
		#although if we have a distributed system all nodes should have their own copy
		
		#res = Agent.get(@stoppers, fn hd -> HashDict.get(hd, h) end)
		unless nil do
			#we have a stopper
			:gen_server.cast(:WFL, {:push, h})
		end
		
		tokenise(t)
	end
end
