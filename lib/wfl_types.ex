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
		defstruct([token: "" , token_count: 0, offset: 0, char_type: :none, defs: [], period_count: 0, punct_len: 0])
	end

	defmodule ReaderInfo do
		defstruct([sent_index: 0, token_info: %TokenInfo{}, sentence_info: %SentenceInfo{}, sentences: []])  #sentences is an array of %SentenceInfo
	end

	defmodule Concretisation do
		defstruct([pid: nil, token_id: nil])
	end

	defmodule RootInfo do
		defstruct([valid: true, freq: 0, conc: %Concretisation{}])
	end

	defmodule WFL_Type do
		defstruct([:type, :type_id, :freq, spaces: 0, root_info: %RootInfo{}, is_common: false, instances: [], concretisations: nil])  #concretisations holds token_ids of types that extend the current type ie catsat extends cat and sat])
	end

	defmodule TokenInstance do
		defstruct([:sentence_id, :offset])
	end

	defmodule TokenInput do
		defstruct([:token, :instance])
	end

	defmodule WFL_Data do
		defstruct([depth: 0, types: %{}, type_ids: %{}])	#both types and type_ids map into the same WFL_Type collections
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
