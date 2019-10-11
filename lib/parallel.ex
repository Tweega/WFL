 defmodule Parallel do
  #http://www.selectedintelligence.com/page/3
  def map(collection, timeout \\ 1000, function) do
    me = self()
    collection
      |> Enum.map(fn(elem) ->

          # Here I spawn a result receiving process which will either
          # timeout or return a result to the original caller, i.e.
          # the parallel map function itself
          result_pid = spawn fn -> (
            receive do {_pid, result} ->
              send me, {self(), {result}}
            after timeout ->
              # I also send back the original element with the
              # error. This is optional: it just helps to show that
              # the results are returned in the correct order
              send me, {self(), {elem, {:error, :processfailed}}}
            end)
          end

          # Here I spawn the process that will run the actual
          # transformation function. When it is done it sends
          # a message back to the result receiver that has just been
          # spawned above. It identifies the result receiver by
          # result_pid. If the result receiver has timed out then
          # that message is cast into nowhere.
          spawn fn -> (
            send result_pid, {self(), function.(elem)})
          end

          # Finally for each element of the collection
          # I return the pid of the result receiver.
          result_pid

        end)
        # The next map function receives a list of the pids
        # of the result receiver processes
      |> Enum.map(fn(pid) ->
          # For each result receiver pid (sequentially) this block
          # receives the actual result, whether it be the timeout
          # message or the successfully returned value
          receive do {^pid, result} ->
            result
          end
        end)
  end

  def pjob_orig(collection, job_list) do
    collection
        |> Enum.map(fn(data_item) -> #assuming that job list is serial not parralel.
          root_pid = self()
          #spawn a listener that will also kick off jobs
          listener_pid = spawn fn ->
            listener = self()
            spawn fn ->
              output = List.foldl(job_list, data_item, fn({module, func, params}, previous_output) ->
                args = [previous_output | params]
                apply(module, func, args)
              end)
              #worker chain has now finished - notify the listener
              send listener, {self(), output}
            end
            #now listen for worker process completion
            receive do {_worker_pid, result} ->
              #explicitly stop the workers?
              send root_pid, {self(), result}
            end
          end

          listener_pid
        end)

       |> Enum.map(fn(listener) ->  #this creates a collection of listeners to the listener pids created above (and who are doing the actual work)
          receive do {^listener, result} ->
            result
          end
      end)
  end



  def pjob(collection, job_list) do
    #IO.inspect(Kernel.map_size(collection))
    collection
    |> Enum.each(fn(data_item) ->
                _output = List.foldl(job_list, data_item, fn({module, func, params}, previous_output) ->
                args = [previous_output | params]
                apply(module, func, args)
              end)
     end)
  end


  def g_stage(collection) do
    {:ok, _p_pid} = GenstageExample.Producer.start_link(collection)


    {:ok, pid} = GenstageExample.ProducerConsumer.start_link()
    # ref = Process.monitor(pid)
    consume(pid, 4)
    #_c = GenstageExample.Consumer.start_link()

    # # Wait for a down message for given ref/pid
    # receive do
    #   {:DOWN, ^ref, :process, ^pid, :normal} ->
    #     IO.puts "Normal exit from #{inspect pid}"
    #   {:DOWN, ^ref, :process, ^pid, msg} ->
    #     IO.puts "Received :DOWN from #{inspect pid}"
    #     IO.inspect msg
    #   xx ->
    #     IO.inspect ({:xx, xx})
    # end

    :okay
  end

  def consume(pc_pid, num_consumers) do
    z =
    1..num_consumers
    |> Enum.map(fn (_elem) ->
      if Process.alive?(pc_pid) do
        #IO.puts("jj")
        case GenstageExample.Consumer.start_link() do
          {:ok, c_pid} ->
            c_pid
          _ ->
            nil
        end
      else
        nil
      end
    end)
    |> Enum.map(fn (pid) ->
      msg =
      if pid != nil && Process.alive?(pid) do
        ref = Process.monitor(pid)
        IO.inspect({:pid, pid})
        m =
        receive do
          {:DOWN, ^ref, :process, ^pid, :normal} ->
            "Normal exit from #{inspect pid}"
          {:DOWN, ^ref, :process, ^pid, msg} ->
            "Received :DOWN from #{inspect pid}.  msg: #{inspect msg} "
          _xx ->
            "eh?"
        end
        m
      else
        "already closed"
      end
      msg
    end)
    IO.inspect({:z, z})
  end

end
