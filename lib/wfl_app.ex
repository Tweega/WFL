defmodule WFLScratch do
use Application

	def start(_type, stack) do
		WFLScratch.Supervisor.start_link(stack)	#start_link is our own code - not part of supervisor
	end

end
