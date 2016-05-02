defmodule WFLScratch.Server do
	use GenServer
	@name :WFL
	
	#API
	#we only have one of these? this creates a text reader for each file (could be handed an array of files or a wildcard file path)
	#and then collates all the results

	def processFile(filePath, readerModule) do
		:gen_server.cast(:WFL, {:wfl_file, {filePath, readerModule}})
	end

	def start_link(_x) do 	#we could initialise with an existing wfl or lemma file? if so we could spawn the process that reads those in.
		:gen_server.start_link({:local, @name}, __MODULE__, %{readers: []}, [])
	end


	#Server
	def init(%{} = state) do
		#stack is map with empty list of readers as its only element.
		{:ok, state}
	end

	
	def handle_cast( {:wfl_file, {filePath, readerModule}}, %{readers: readers} = state) do
		process_file(filePath, readerModule, readers)
		{:noreply, state}
	end

	def handle_info( {:file_complete, _filePath}, state) do
		IO.puts "Handle info: File read: complete"
		{:noreply, state}
	end

	def handle_info( {:file_error, _filePath}, state) do
		IO.puts "Handle info File error: Something else"
		{:noreply, state}
	end

	defp process_file(filePath, readerModule, _readers) do
		#here we create a text-reader process and get it to report back every so often with tokens/sentences
		wfl_pid = 123	#this will either come from outside, or need tobe created at startup
		char_def_tree = CharClass.new()
		me = self()
		apply(readerModule, :processText, [filePath, wfl_pid, char_def_tree, me])	#handle_info will be called when finished
		
		#readers = [reader | readers]
		
	end
end
