defmodule WFLScratch.Supervisor do
	use Supervisor

	def start_link(stack) do
		 :supervisor.start_link(__MODULE__, stack)
	end

	def init (stack) do
		children = [worker(WFLScratch.Server, [stack]),
					worker(SentenceCounter, []),
					worker(Sentences, []),
					worker(PhraseCounter, []),
					worker(TokensBinary, []),
					#worker(Expansion, []),
					worker(ProcessedPhrases, []),
					worker(TokenCounter, []),
					worker(NamedWFL, []),
					worker(Concretiser, [])]
		supervise children, strategy: :one_for_one
	end
end
