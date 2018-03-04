defmodule WflScratchTest do
  use ExUnit.Case
  import PostgrexHelper
  #import ExUnit.CaptureLog
  alias Postgrex, as: P

  #doctest WflScratch

  setup context do
    opts = [database: "postgrex_test", backoff_type: :stop,
            prepare: context[:prepare] || :named, user: "christopher", password: "Ngorogor0", pool: DBConnection.Poolboy]
    {:ok, pid} = P.start_link(opts)
    {:ok, [pid: pid, options: opts]}
  end


  test "the truth", context do
    IO.inspect(context)
    assert 1 + 1 == 2
  end
end
