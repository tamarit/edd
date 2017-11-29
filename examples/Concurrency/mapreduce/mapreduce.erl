% Author: Jesús Doménech
% Asignatura: Programación Declarativo Aplicada

-module(mapreduce).
% atajos para ejemplos
-export([ejemplo1/0,fmap1/2,freduce1/3,client1/1]).
% funciones de master
-export([master/2]).
% funciones de nodo
%-export([node_map/3]).
-export([init/1]).
-compile(export_all).


start(Info,N) ->
    Mpid = spawn(?MODULE, master, [Info,N] ),
    Mpid.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% master functions
master(Info, N) ->
    Self = self(),
    Parts = split_N(Info,N),
    Nodes = lists:map(fun(Id) ->
			      node_map(Id,Self,lists:nth(Id,Parts))				
		      end,lists:seq(1,length(Parts))),
    loop_master(Parts, N, Nodes).

loop_master(Info, N, Nodes) ->
    receive 
	stop ->
	    stop;
	{From,{mapreduce, Parent, FMap, FReduce}} ->
	    From ! {self(),{admin,mradmin(Nodes,Parent,FMap,FReduce)}},
	    loop_master(Info, N, Nodes);
	X ->
	    io:format("~p~n",[X]),
	    loop_master(Info, N, Nodes)
    end.
    
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% node functions

node_map(Id,Master,Info) ->
    spawn(?MODULE, loop_map, [Id,Master,Info] ).
loop_map(Id,Master,Info) ->
    Self = self(),
    receive 
	stop ->
	    stop;
	{From,{startmap,From,FMap}} ->
	    spawn(fun() ->
			  lists:map(fun(Part) ->
					    List = FMap(Id,Part),
					    lists:map(fun({C,V}) ->
							      From ! {Self,{C,V}}
						      end,List)
				    end,Info),
			  From ! {Self,{fin}}
		  end);
		
	X ->
	    io:format("Map...: ~p~n",[X]),
	    loop_map(Id,Master,Info)
    end.
    

node_reduce(Mradmin,FReduce,Clave,Valor) ->
    spawn(?MODULE, loop_reduce, [Mradmin,FReduce,Clave,Valor] ).
loop_reduce(Mradmin,FReduce,Clave,ValorActual) ->
    receive 
	stop ->
	    stop;
	{_From,{newvalue,Valor}} ->
	    V = FReduce(Clave,ValorActual,Valor),
	    loop_reduce(Mradmin,FReduce,Clave,V);
	{_From,{fin}} ->
	    Mradmin ! {self(),{Clave,ValorActual}};
	X ->
	    io:format("Reduce...: ~p~n",[X]),
	    loop_reduce(Mradmin,FReduce,Clave,ValorActual)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% mradmin functions

mradmin(Nodes,Client,FMap,FReduce) ->
    spawn(fun() ->
		  send_all(Nodes,{self(),{startmap,self(),FMap}}),
		  Dict = dict:new(),
		  {State,R} = loop_admin_map(Dict,length(Nodes),0,FReduce),
		  case State of
		      error ->
			  send_all(Nodes,stop),
			  io:format("Error MAP: ~p~n",[R]);
		      ok ->
			  send_all(R,{self(),{fin}}),
			  {S2,R2} = loop_admin_reduce(length(R),[]),
			  case S2 of 
			      error ->
				  send_all(R,stop),
				  io:format("Error Reduce: ~p~n",[R2]);
			      ok ->
				  Client ! {self(),{R2}}
			  end
		  end
	  end).

loop_admin_map(Dict,Nmappers,Nreducers,FReduce) ->
    receive
	{_From,{Clave,Valor} }->
		case dict:is_key(Clave, Dict) of
		    true ->
			Pid = dict:fetch(Clave,Dict),
			Pid ! {self(),{newvalue,Valor}},
			loop_admin_map(Dict,Nmappers,Nreducers,FReduce);
		    false ->
			Pid = node_reduce(self(),FReduce,Clave,Valor),
			D2 = dict:store(Clave,Pid,Dict),
			loop_admin_map(D2,Nmappers,Nreducers+1,FReduce)
		end;
	{_From,{fin}} -> 
	    N = Nmappers - 1,
	    case N =< 0 of
		true -> 
		    Pids = lists:map(fun({_K,V}) -> V end, dict:to_list(Dict)),
		    {ok,Pids};
		false ->
		    loop_admin_map(Dict,N,Nreducers,FReduce)
	    end;
	X ->
	    Pids = lists:map(fun({_K,V}) -> V end, dict:to_list(Dict)),
	    send_all(Pids,stop),
	    {error,{"Msg",X}}
    end.

loop_admin_reduce(Nreducers,Result) ->
    receive
	{_From,{Clave,Valor}} ->
	    R2 = Result++[{Clave,Valor}],
	    N = Nreducers - 1,
	    case N =< 0 of
		true ->
		    {ok,R2};
		false ->
		    loop_admin_reduce(N,R2)
	    end;
	X ->
	    {error,{"Msg",X}}
    end.
			

	    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% misc functions
% Funciones que no tienen que ver directamente con mapReduce

% divide L en N partes iguales, si Length(L) < N serán length(L) partes y no N
% EM: divide L en N trozos. La diferencia de tamaños sera +/- 1
split_N(L,N) ->
%    Len = ceil(length(L)/N),
%    split(L,Len).
  Pos = lists:seq(0, length(L)-1),
  Zip = lists:zip(L, Pos),
  split(Zip, 0, N).
  
split(L, Pos, N) when Pos == N-1 ->
  {LL,_} = lists:unzip(L),
  [LL];
split(L, Pos, N) when Pos < N-1 ->  
  {PartPos, Next} = lists:partition( fun({_,T}) -> T rem N == Pos end, L ),
  Rest = split(Next, Pos+1, N),
  {PartPosUZ,_} = lists:unzip(PartPos),
  [PartPosUZ | Rest].
  

% divide L en partes de tamaño N
%split([], _) -> [];
%split(L, N) when length(L) < N -> [L];
%split(L, N) ->
%    {A, B} = lists:split(N, L),
%    [A | split(B, N)].

% EM: no es necesaria, es una BIF
% aproxima a entero superior
%ceil(X) when X < 0 ->
%    trunc(X);
%ceil(X) ->
%    T = trunc(X),
%    if X == T -> T;
%       true -> T + 1
%    end.

send_all(List,Msg) ->
    lists:map(fun(Pid) ->
		      Pid ! Msg
	      end,List).

% init breve para lanzar el ejemplo
ejemplo1() ->
    start([{madrid,34},{barcelona,21},{madrid,22},{barcelona,19},{teruel,-5}, {teruel, 14}, {madrid,37}, {teruel, -8}, {barcelona,30},{teruel,10}],3).


fmap1(_Id,Part) ->
    %io:format("DEBUG: id: ~p, Part: ~p ~n",[Id,Part]),
    lists:filter(fun({_Ciudad,Grados}) ->
			 Grados >= 28 end, [Part]).

freduce1(_Clave,ValorActual,ValorNuevo) ->
    %io:format("DEBUG: Clave: ~p, VA: ~p, VN: ~p~n",[Clave,ValorActual,ValorNuevo]),
    % max(ValorActual,ValorNuevo). % OK
    min(ValorActual,ValorNuevo).   % BUG


client1(Master) ->
    % EM: quito el spawn
    %spawn(fun() ->
		  %Master ! {self(),{mapreduce,self(),fun(A,B) -> fmap1(A,B) end,fun(A,B,C) -> freduce1(A,B,C) end}},
		  Master ! {self(),{mapreduce,self(),fun ?MODULE:fmap1/2, fun ?MODULE:freduce1/3}},
		  receive
		      {Master,{admin,Pid}} ->
			  receive
			      {Pid,X}->
				  %io:format("RESULT: ~p~n",[X])
				        X
			  end
		  end.
	  %end).
	  

%EM: predicado inicial
init(N) -> 	  
    % Lista con 20 mediciones de temperatura
    % L = [{madrid,34}, {barcelona,21}, {madrid,22}, {barcelona,19}, {teruel,-5}, {teruel, 14}, {madrid,37}, {teruel, -8}, {barcelona,30}, {teruel,10}, 
    %     {madrid,34}, {barcelona,21}, {madrid,22}, {barcelona,19}, {teruel,-5}, {teruel, 14}, {madrid,37}, {teruel, -8}, {barcelona,30}, {teruel,10}],
    L = [{madrid,37}, {madrid,34}, {barcelona,19}, {teruel,-5}, {teruel, 14}, {barcelona,22}, {teruel, -8}, {barcelona,17}, {teruel,10}, {teruel,11}],
    Master = start(L, N),
    R = client1(Master),
    Master ! stop,
    R.
