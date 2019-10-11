defmodule WFLScratch.Supervisor do
	use Supervisor

	def start_link(stack) do
		 :supervisor.start_link(__MODULE__, stack)
	end

	def init (stack) do

		opts = []
			|> Keyword.put_new(:username, "christopher")
			|> Keyword.put_new(:password, "Ngorogor0")
			|> Keyword.put_new(:database,"wfl_dev")
			|> Keyword.put_new(:hostname, "localhost")
			|> Keyword.put_new(:port, 5432)
			|> Keyword.put_new(:types, WFLScratch.PostgrexTypes)
#			|> Keyword.put_new(:pool, DBConnection.Poolboy)
			|> Keyword.put_new(:name, :pgpg)



		children = [worker(WFLScratch.Server, [stack]),
					worker(SentenceCounter, []),
					worker(Sentences, []),
					worker(PhraseCounter, []),
					worker(TokensBinary, []),
					#worker(Expansion, []),
					worker(ProcessedPhrases, []),
					worker(TokenCounter, []),
					worker(Concretiser, []),
					Postgrex.child_spec(opts),
					#worker(GenstageExample.Producer, [0]),
					#worker(GenstageExample.ProducerConsumer, []),
					#worker(GenstageExample.Consumer, [])
				]
		supervise children, strategy: :one_for_one
	end
end
