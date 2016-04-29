defmodule CharClass do
require Logger

def new() do

BTree2.new(%{range: 97..122, type: :letter},  &Utils.compare_char_type/2)
	|> BTree2.insert(%{range: 32..32, type: :ws})
	|> BTree2.insert(%{range: 65..90, type: :letter})
	|> BTree2.insert(%{range: 40..44, type: :punct})
	|> BTree2.insert(%{range: 46..46, type: :period})
	|> BTree2.insert(%{range: 48..57, type: :number})
	|> BTree2.insert(%{range: 33..33, type: :stop})
	|> BTree2.insert(%{range: 63..63, type: :stop})
	|> BTree2.insert(%{range: 34..34, type: :quot})
	|> BTree2.insert(%{range: 39..39, type: :apos})
	|> BTree2.insert(%{range: 9..10, type: :ws})
	|> BTree2.insert(%{range: 192..214, type: :letter})
	|> BTree2.insert(%{range: 216..246, type: :letter})
	|> BTree2.insert(%{range: 248..255, type: :letter})
	|> BTree2.insert(%{range: 45..45, type: :hyphen})
	|> BTree2.insert(%{range: 35..38, type: :punct})
	|> BTree2.insert(%{range: 40..44, type: :punct})
	|> BTree2.insert(%{range: 47..47, type: :punct})
	|> BTree2.insert(%{range: 58..62, type: :punct})
	|> BTree2.insert(%{range: 64..64, type: :punct})
	|> BTree2.insert(%{range: 91..96, type: :punct})
	|> BTree2.insert(%{range: 123..126, type: :punct})
	|> BTree2.insert(%{range: 161..172, type: :punct})
	|> BTree2.insert(%{range: 215..215, type: :punct})
	|> BTree2.insert(%{range: 247..247, type: :punct})
	|> BTree2.insert(%{range: 13..13, type: :ws})
	|> BTree2.insert(%{range: 160..160, type: :punct}) #nbs
	|> BTree2.insert(%{range: 8192..8303, type: :punct}) #html5 punct
	

 end

end