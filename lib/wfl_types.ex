	defmodule SentenceInfo do
		#this struct is used when reading text file initially
		defstruct([tokens: [], sentence: <<>>])
	end

	defmodule BinTokens do
		#this is an expansion of all tokens in a sentence - 4 bytes per token - including amalgamated tokens.

		defstruct(bin_tokens: <<>>)
	end

	defmodule Paragraph do
		defstruct(sentences: [])	#list of SentenceInfo
	end

	defmodule TokenInfo do
		defstruct([token: "" , token_count: 0, char_type: :none, defs: [], period_count: 0, punct_len: 0])
	end

	defmodule ReaderInfo do
		defstruct([token_info: %TokenInfo{}, sentence_info: %SentenceInfo{}, sentences: []])  #sentences is an array of %SentenceInfo
	end

	defmodule WFL_Type do
		@derive {Poison.Encoder, only: [:type, :freq]}
		defstruct([:type, :type_id, :freq, is_common: false, instances: [], concretisations: []])  #concretisations holds token_ids of types that extend the current type ie catsat extends cat and sat])

		defimpl Poison.Encoder, for: WFL_Type do
			def encode(data, options) do
				Poison.Encoder.Map.encode(Map.take(data, [:type, :freq]), options)
			end
		end

	end

	defmodule TokenInstance do
		defstruct([:sentence_id, :offset])
	end

	defmodule TokenInput do
		defstruct([:token, :instance])
	end

	defmodule WFL_Data do
		@derive {Poison.Encoder, only: [:types]}
		defstruct([depth: 0, types: %{}, type_ids: %{}])	#both types and type_ids map into the same WFL_Type collections

		defimpl Poison.Encoder, for: WFL_Data do
    def encode(data, options) do
			IO.inspect({:hello})
      Poison.Encoder.Map.encode(Map.take(data, [:types]), options)
    end
  end

	end

	defmodule Check_Type do
		def check(%TokenInput{} = struc) do
			IO.puts("TokenInput")
			struc
		end

		def check(%SentenceInfo{} = struc) do
			IO.puts("SentenceInfo")
			struc
		end

		def check(%WFL_Type{} = struc) do
			IO.puts("WFL_Type")
			struc
		end

		def check(%WFL_Data{} = struc) do
			IO.puts("WFL_Data")
			struc
		end
	end
