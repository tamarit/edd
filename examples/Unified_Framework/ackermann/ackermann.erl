-module(ackermann).
%-export([main/1]).
-compile(export_all).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

main( [A, B] ) ->
%   io:fwrite("~p~n",[ack(A,B)]).
  ack(A,B).
 
%toi(E) -> element(1,string:to_integer(E)).
 
ack(0,N) -> N + 1;
ack(M,0) -> %ack(M-1, 1); %RIGHT
	          ack(M-1, 0); %WRONG
ack(M,N) -> ack(M-1,ack(M,N-1)).


%% Tests
ack0_property() ->
  ?FORALL(N, non_neg_integer(), ack(0, N) =:= N + 1).
  
ack1_property() ->
  ?FORALL(N, non_neg_integer(), ack(1, N) =:= N + 2).
  
%ack2_property() ->
%  ?FORALL(N, non_neg_integer(), ack(2, N) =:= 2*N + 3).

