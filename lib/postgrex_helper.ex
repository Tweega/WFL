defmodule PostgrexHelper do
  def query(stat, params, opts \\ []) do

      #opts = Keyword.put_new(opts, :pool, DBConnection.Poolboy)
      #IO.inspect({:opts, opts})
      pid = Process.whereis(:pgpg)
      xx = Postgrex.query(pid, stat, params, opts)
       #IO.inspect (xx)
      case xx do
        {:ok, %Postgrex.Result{rows: nil}} -> :ok
        {:ok, %Postgrex.Result{rows: [[:void]], num_rows: num_rows}} -> {:ok, num_rows}
        {:ok, %Postgrex.Result{rows: rows}} -> rows
        {:error, %Postgrex.Error{} = err} -> err
      end

  end

  def prepare(name, stat, opts \\ []) do
    #opts = Keyword.put_new(opts, :pool, DBConnection.Poolboy)
    pid = Process.whereis(:pgpg)
      case Postgrex.prepare(pid, name, stat, opts) do
        {:ok, %Postgrex.Query{} = query} -> query
        {:error, %Postgrex.Error{} = err} -> err
      end

  end

  def execute(query, params, opts \\ []) do
    #opts = Keyword.put_new(opts, :pool, DBConnection.Poolboy)
    pid = Process.whereis(:pgpg)
      case Postgrex.execute(pid, query,
                                       params, opts) do
        {:ok, %Postgrex.Result{rows: nil}} -> :ok
        {:ok, %Postgrex.Result{rows: rows}} -> rows
        {:error, %Postgrex.Error{} = err} -> err
      end


  end


  def close(query, opts \\ []) do
    pid = Process.whereis(:pgpg)
      case Postgrex.close(pid, query,
                                     opts) do
        :ok -> :ok
        {:error, %Postgrex.Error{} = err} -> err
      end

  end

  def transaction(fun, opts \\ []) do
    pid = Process.whereis(:pgpg)
      Postgrex.transaction(pid, fun,
                                      opts)

  end

  def getAddPhraseQuery() do
    sql = "select staging.add_phrase_candidate2($1::bigint, $2::int, $3::jsonb)"
    prepare("add_phrase", sql)
    #PostgrexPreparedQuery.set_query_by_name("add_phrase", q)
  end
end
