defmodule PostgrexHelperMacros do
  defmacro query(stat, params, opts \\ []) do
    quote do
      opts = Keyword.put_new(unquote(opts), :pool, DBConnection.Poolboy)
      #IO.inspect({:opts, opts})

      case Postgrex.query(var!(context)[:pid], unquote(stat),
                                     unquote(params), opts) do
        {:ok, %Postgrex.Result{rows: nil}} -> :ok
        {:ok, %Postgrex.Result{rows: rows}} -> rows
        {:error, %Postgrex.Error{} = err} -> err
      end
    end
  end

  defmacro prepare(name, stat, opts \\ []) do
    quote do
      case Postgrex.prepare(var!(context)[:pid], unquote(name),
                                     unquote(stat), unquote(opts)) do
        {:ok, %Postgrex.Query{} = query} -> query
        {:error, %Postgrex.Error{} = err} -> err
      end
    end
  end

  defmacro execute(query, params, opts \\ []) do
    quote do
      case Postgrex.execute(var!(context)[:pid], unquote(query),
                                       unquote(params), unquote(opts)) do
        {:ok, %Postgrex.Result{rows: nil}} -> :ok
        {:ok, %Postgrex.Result{rows: rows}} -> rows
        {:error, %Postgrex.Error{} = err} -> err
      end
    end
  end

  defmacro stream(query, params, opts \\ []) do
    quote do
      Postgrex.stream(var!(conn), unquote(query), unquote(params), unquote(opts))
    end
  end

  defmacro close(query, opts \\ []) do
    quote do
      case Postgrex.close(var!(context)[:pid], unquote(query),
                                     unquote(opts)) do
        :ok -> :ok
        {:error, %Postgrex.Error{} = err} -> err
      end
    end
  end

  defmacro transaction(fun, opts \\ []) do
    quote do
      Postgrex.transaction(var!(context)[:pid], unquote(fun),
                                      unquote(opts))
    end
  end
end
