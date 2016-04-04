
defmodule Pairs do
require Logger

def draw_pairs() do

	female = Enum.shuffle(["Gill Hogan", "Brenda Postens", "Paulette   Hunte", "Karen Clayton", "Kimberley Young", "Susan Moore", "Lyn Kirby", "Sarah Mucott", "Denise Grundy", "Sandra Jones", "Len Tiu Wright", "Sue Key", "Sally Morgan", "Tina Olner", "Sam Revitt", "Jackie Phillips"])

	male = Enum.shuffle(["Adrian Ruck", "Jon Bache", "Len Tiu Wright", "Lawrence Ormond", "Matt Heap", "Terry Hamond", "Jerry Rowe", "John Key", "Pete Sutton", "Yogesh Gupta", "Tom Revitt", "Mike Phillips", "Anil Kadara"])

	draw_pairs(male, female, []) 
end



def draw_pairs([male | males], [female | females], acc) do
	
	draw_pairs(males, females, [{male, female} | acc])
end

def draw_pairs([], [female | []], acc) do
	[{female, "sit out"} | acc]
end

def draw_pairs([], [female, female2 | females], acc) do
	draw_pairs([], females, [{female, female2} | acc])
end


end