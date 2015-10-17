-module(quicksort).
-include_lib("eunit/include/eunit.hrl").
-export([qs/2, leq/2]).


qs(_, []) -> [];
qs(F, [E|R]) ->
  {A, B} = partition(F,E,R),
  %%%% Bug 1 %%%%
  qs(F,B) ++ [E] ++ qs(F,A). % Bug
% qs(F,A) ++ [E] ++ qs(F,B). % OK
  %%%%%%%%%%%%%%%


partition(_, _, []) -> {[],[]};
partition(F, E, [H|T]) ->
  {A,B} = partition( F, E, T ),
  case F(H,E) of
    true  -> {[H|A],B}; 
    %%%% BUG 2 %%%%
%   false -> {A, [H|B]} % OK
    false -> {A,B}      % Bug
    %%%%%%%%%%%%%%%
  end.


leq( A, B ) -> 
  A =< B.


quicksort_test() ->
	?assert( [] == qs( fun leq/2, [] ) ),
	?assert( [1] == qs( fun leq/2, [1] ) ),
	?assert( [1,7] == qs( fun leq/2, [7,1] ) ),
  ?assert( [1,7,8] == qs( fun leq/2, [7,8,1] ) ).
