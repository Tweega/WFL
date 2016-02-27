defmodule BTree2 do
  defstruct tree: nil, compare: nil

  def new(e, f \\ &BTree2.compare/2) do
    %BTree2{tree: {e, nil, nil}, compare: f}
  end

  def find(%BTree2{tree: root, compare: f}, element) do
    do_find(root, element, f)
  end

  defp do_find(tuple, element, _f) when is_nil(tuple) do    
    %{range: element..element, type: :unknown}
  end

  defp do_find({e, l, r}, element, f) do
    
    cmp = apply(f, [e, element])

    cond do
      cmp < 0 -> #element smaller than e - go left
        do_find(l, element, f)
      cmp > 0 -> #go right
        do_find(r, element, f)
      true ->
        e  w# e.g. %{range: 65..90, type: :letter}
        
    end
  end


  
  def insert(%BTree2{tree: root, compare: f}, element) do
    %BTree2{tree: do_insert(root, element, f), compare: f}
  end

  defp do_insert(nil, element, _f) do  #l or r was nil, not a tuple
    {element, nil, nil}
  end

  defp do_insert({e, l, r} = tuple, element, f) do
    comp = apply(f, [e, element])

    cond do
      comp < 0 ->
        {e, do_insert(l, element, f), r}
      comp > 0 ->
        {e, l, do_insert(r, element, f)}
      true -> tuple
    end    
  end

  def compare(a, b) do
    cond do
      a > b -> -1
      b > a -> 1
      true -> 0
    end
  end
end 
