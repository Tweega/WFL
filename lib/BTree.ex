defmodule BTree do
  defstruct tree: nil

  def new(e), do: %BTree{tree: {e, nil, nil}}

  def insert(%BTree{tree: root}, element) do
    %BTree{tree: do_insert(root, element)}
  end

  defp do_insert(nil, element) do
    {element, nil, nil}
  end

  defp do_insert({e, l, r}, element) when e > element do
    {e, do_insert(l, element), r}
  end

  defp do_insert({e, l, r}, element) when e < element do
    {e, l, do_insert(r, element)}
  end

  defp do_insert({_, _, _} = tuple, _element) do
    tuple
  end

end 
