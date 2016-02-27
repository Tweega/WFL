defmodule WFLScratch.Stopper do

	@name :JO

	##
	# External API

	def start_link do
		res = Agent.start_link(fn -> HashDict.new end, name: @name)	#with a fixed name there will only be one process started
		set_stoppers(".!?")	#this should come from config file or data store
		res
	end


	defp set_stoppers(stoppers) do
		#expect stoppers to be a charlist
		process_stoppers(String.to_char_list(stoppers))
	end

	defp process_stoppers([]) do
		#no more stoppers
	end

	defp process_stoppers([stopper|stoppers]) do
		Agent.update(@name, fn stop_list -> HashDict.put(stop_list, stopper, stopper) end) #is there a container that only stores keys?
		process_stoppers(stoppers)
	end


end