-module(sum_digits).
%-export([sum_digits/2, sum_digits/1]).
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
    

%% Tests %%       
sum_digits_property_complete() ->
  ?FORALL({N, Acc},
          {integer(0,9), non_neg_integer()}, 
          sum_digits(N, 10, Acc) =:= N + Acc).

