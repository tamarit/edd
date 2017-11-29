%% Dining philosophers from Rosetta Code
%%   https://rosettacode.org/wiki/Dining_philosophers#Erlang
%% Extended to accept any number of philosophers and interactions; and use 
%% timer:sleep

-module(philosophers).
-compile(export_all).


-define(WAITING_MILLIS, 50).

 
doForks(ForkList) ->
  receive
	{grabforks, {Left, Right}} -> doForks(ForkList -- [Left, Right]);
	{releaseforks, {Left, Right}} -> doForks([Left, Right| ForkList]);
	{available, {Left, Right}, Sender} -> 
          Sender ! {areAvailable, lists:member(Left, ForkList) andalso lists:member(Right, ForkList)},
          doForks(ForkList);
	{die} -> 
	        io:format("Forks put away.~n"),
	        unregister(forks)
  end.
 
areAvailable(Forks) ->
	forks ! {available, Forks, self()},
	receive
		{areAvailable, B} -> B
	end.
 
 
processWaitList([]) -> false;
processWaitList([H|T]) ->
	{Client, Forks} = H,
	case areAvailable(Forks) of
		true -> Client ! {served},
			true;
		false -> processWaitList(T)
	end.
 
doWaiter([], 0, 0, false) -> 
	forks ! {die},
	io:format("Waiter is leaving.~n"),
	unregister(waiter),
	diningRoom ! {allgone};
doWaiter(WaitList, ClientCount, EatingCount, Busy) ->
	receive
		{waiting, Client} ->
			WaitList1 = [Client|WaitList],	%% add to waiting list
			case (not Busy) and (EatingCount<2) of	
				true ->	Busy1 = processWaitList(WaitList1);
				false -> Busy1 = Busy
			end,
			doWaiter(WaitList1, ClientCount, EatingCount, Busy1);
 
		{eating, Client} ->
			doWaiter(WaitList -- [Client], ClientCount, EatingCount+1, false);		
 
		{finished} ->
			doWaiter(WaitList, ClientCount, EatingCount-1, 
				processWaitList(WaitList)); 
		{leaving} ->
			doWaiter(WaitList, ClientCount-1, EatingCount, Busy)
	end.
 
 
philosopher(Name, _Forks, 0) -> 	
	io:format("~p is leaving.~n", [Name]),
	waiter ! {leaving};
 
 
philosopher(Name, Forks, Cycle) ->
	io:format("~p is thinking.~n", [Name]),
	timer:sleep(rand:uniform(?WAITING_MILLIS)),
 
	io:format("~p is hungry.~n", [Name]),
	waiter ! {waiting, {self(), Forks}}, %%sit at table
 
	receive
		{served}-> forks ! {grabforks, Forks},	%%grab forks
			waiter ! {eating, {self(), Forks}},	%%start eating
			io:format("~p is eating.~n", [Name])
	end,
 
	timer:sleep(rand:uniform(?WAITING_MILLIS)),
	forks ! {releaseforks, Forks},					%% put forks down
	waiter ! {finished},
 
  %philosopher(Name, Forks, Cycle-1).  % OK
  philosopher(Name, Forks, Cycle-2).   % BUG

 
 
dining() ->	
    AllForks = [1, 2, 3, 4, 5],
		Clients = 5,
		register(diningRoom, self()),
 
    %register(forks, spawn(fun()-> doForks(AllForks) end)),
    register(forks, spawn(?MODULE, doForks, [AllForks])),
		register(waiter, spawn(fun()-> doWaiter([], Clients, 0, false) end)),
		Life_span = 20,
		spawn(fun()-> philosopher('Aristotle', {5, 1}, Life_span) end),
		spawn(fun()-> philosopher('Kant', {1, 2}, Life_span) end),
		spawn(fun()-> philosopher('Spinoza', {2, 3}, Life_span) end),
		spawn(fun()-> philosopher('Marx', {3, 4}, Life_span) end),
		spawn(fun()-> philosopher('Russel', {4, 5}, Life_span) end),
 
		receive
 			{allgone} -> io:format("Dining room closed.~n")
		end,
		
		unregister(diningRoom).
		
		
spawn_philosophers(N, Clients, _Life_span) when N >= Clients ->
    ok;
spawn_philosophers(N, Clients, Life_span) when N < Clients ->
    spawn(?MODULE, philosopher, [{'Philosopher',N}, {N, (N+1) rem Clients}, Life_span]),
    spawn_philosophers(N+1, Clients, Life_span).
    

%% entry point with parameters		
dining(Clients, Life_span) ->	
    AllForks = lists:seq(0, Clients-1),
		register(diningRoom, self()),
 
		register(forks, spawn(?MODULE, doForks, [AllForks])),
		register(waiter, spawn(?MODULE, doWaiter, [[], Clients, 0, false])),
  
    spawn_philosophers(0, Clients, Life_span),
 
		receive
 			{allgone} -> io:format("Dining room closed.~n")
		end,
		
		unregister(diningRoom).


%% benchmark function
init(N) ->
	dining(N, 2).
