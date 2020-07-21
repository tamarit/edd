-module(sum_digits).
-compile(export_all).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

 
sum_digits(N) ->
    sum_digits(N,10).
 
sum_digits(N,B) ->
    sum_digits(N,B,0).
 
sum_digits(0,_,Acc) ->
    Acc;
sum_digits(N,B,Acc) when N < B ->
    %Acc+N; %RIGHT
    Acc; %WRONG
sum_digits(N,B,Acc) ->
    sum_digits(N div B, B, Acc + (N rem B)).

