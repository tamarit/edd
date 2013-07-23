%edd:dd("ackerman:main([3,4])").
-module(ackerman).
-export([main/1]).

main( [A, B] ) ->
%   io:fwrite("~p~n",[ack(A,B)]).
  ack(A,B).
 
%toi(E) -> element(1,string:to_integer(E)).
 
ack(0,N) -> N + 1;
ack(M,0) -> %ack(M-1, 1); %RIGHT
	          ack(M-1, 0); %WRONG
ack(M,N) -> ack(M-1,ack(M,N-1)).
