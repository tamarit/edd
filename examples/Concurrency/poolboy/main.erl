%% Initial symptoms of poolboy's bugs created from the counterexamples in
%% https://gist.github.com/Vagabond/f12a33b261f18f931014#file_counterexample+1
%% that were obtained using QuickCheck. A complete description can be foun in
%% http://basho.com/posts/technical/quickchecking-poolboy-for-fun-and-profit/

-module(main).

-export([symptom1/0, symptom2/0, symptom3/0, symptom4/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%
%% AUXILIARY FUNCTIONS %%
%%%%%%%%%%%%%%%%%%%%%%%%%

%% Do a blocking checkout and handles the possible timeout exception
blocking_checkout(PoolName) ->
    case catch poolboy:checkout(PoolName, true) of
        {'EXIT',{timeout,_}} -> timeout;
        V -> V
    end.

%% Kill a worker (server) and waits until it finishes
kill_worker(Pid) ->
    erlang:monitor(process, Pid),
    exit(Pid, kill),
    receive
        {'DOWN', _, process, Pid, _} ->
            ok
    end.

%% Send a sycn message to the FSM with timeout 1000ms
%% Catch the possible timeout exception
sync_msg(PoolName, Msg) ->
    case catch gen_fsm:sync_send_all_state_event(PoolName, Msg, 1000) of
        {'EXIT',{timeout,_}} -> timeout;
        V -> V
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%


symptom1() ->
  %% 1. Start poolboy with a size of 0 and an overflow of 1
  {ok, Pid} = poolboy:start_link([{size, 0}, {max_overflow, 1},
    {name, undefined}, {worker_module, poolboy_test_worker}]),
  %% 2. Do a non-blocking checkout, which succeeds
  Worker1 = poolboy:checkout(Pid, false),
  io:format("~p ~n", [Worker1]),
  %% 3. Do a blocking checkout that fails (with a timeout)
  Worker2 = blocking_checkout(Pid),
  io:format("~p ~n", [Worker2]),
  %% 4. Check the worker obtained in step 2 back in
  poolboy:checkin(Pid, Worker1),
  %% 5. Do another non-blocking checkout
  %% The result of step 5 should be a worker, but we get full instead.
  Worker3 = poolboy:checkout(Pid, false),
  io:format("~p ~n", [Worker3]).


symptom2() ->
  %% 1. Start a pool with a size of 1 and overflow of 1
  {ok, Pid} = poolboy:start_link([{size, 1}, {max_overflow, 1},
  {name, undefined}, {worker_module, poolboy_test_worker}]),
  %% 2. Do 3 non-blocking checkouts, first 2 succeed, 
  %% the third rightfully fails
  Worker1 = poolboy:checkout(Pid, false),
  Worker2 = poolboy:checkout(Pid, false),
  Worker3 = poolboy:checkout(Pid, false),
  io:format("~p - ~p - ~p~n", [Worker1, Worker2, Worker3]),
  %% 3. Check both of the workers we successfully checked out back in
  poolboy:checkin(Pid, Worker1),
  poolboy:checkin(Pid, Worker2),
  timer:sleep(500),
  %% 4. Check a worker back out
  Worker4 = poolboy:checkout(Pid, false),
  io:format("~p ~n", [Worker4]),
  %% 5. Kill it while its checked out
  kill_worker(Worker4),
  %% 6. Do 2 more checkouts, both should succeed but 
  %% instead the second one reports the pool is 'full'
  Worker5 = poolboy:checkout(Pid, false),
  Worker6 = poolboy:checkout(Pid, false),
  io:format("~p - ~p ~n", [Worker5, Worker6]).


symptom3() ->
    %% 1. Pool size 2, no overflow
    {ok, Pid} = poolboy:start_link([{name, undefined}, %{local, pool_aritmetico}
                                     {worker_module, poolboy_test_worker},
                                     {size, 2}, {max_overflow, 0}]),

    %% 2. Checkout a worker
    Worker1 = poolboy:checkout(Pid, false),
    io:format("~p ~n", [Worker1]),

    %% 3. Kill an idle worker (check it out, check it back in and then kill it)
    Worker2 = poolboy:checkout(Pid, false),
    io:format("~p ~n", [Worker2]),

    poolboy:checkin(Pid, Worker2),

    kill_worker(Worker2),

    %% Get all monitors. Poolboy wasn’t monitoring any processes, when clearly 
    %% it should have been monitoring who had done the checkout in step 2. 
    sync_msg(Pid, get_all_monitors).
    
    %% 4. Checkout a worker -> not needed


symptom4() ->
    %% 1. Pool size 1, no overflow
    {ok, Pid} = poolboy:start_link([{name, undefined}, %{local, pool_aritmetico}
                                     {worker_module, poolboy_test_worker},
                                     {size, 1}, {max_overflow, 0}]),

    %% 2. Checkout a worker
    Worker1 = poolboy:checkout(Pid, false),
    io:format("~p ~n", [Worker1]),

    %% 3. Send a spurious EXIT message
    Pid ! {'EXIT', list_to_pid("<0.4.1>"), spurious},
    
    %% 4. Kill the worker we checked out
    kill_worker(Worker1),
    
    %% Get available workers
    %% poolboy thought it had 2 workers available, not one
    sync_msg(Pid, get_avail_workers).

    %% 5. Stop the pool -> not needed

