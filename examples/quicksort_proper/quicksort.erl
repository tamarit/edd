-module(quicksort).
% -export([quicksort/2, partition/3, quicksort_proper_complete/0, quicksort_sorted_proper/0, quicksort_length_proper/0, partition_split_proper/0, partition_length_proper/0, leq/2]).

-compile(export_all).

-include_lib("proper/include/proper.hrl").
%% El orden de inclusiones es importante porque proper redefine el ?LET de EUnit
-include_lib("eunit/include/eunit.hrl"). 


quicksort(_, []) -> [];
quicksort(Order, [Pivot|R]) -> 
  {Less, Greater} = partition(Order, Pivot, R), 
  %quicksort(Order, Greater) ++ [Pivot] ++ quicksort(Order, Less). %bug1
  quicksort(Order, Less) ++ [Pivot] ++ quicksort(Order, Greater). %OK

leq(A, B) -> A =< B.

partition(_, _, []) -> {[], []}; 
partition(Order, Pivot, [H|T]) -> 
  {Less, Greater} = partition(Order, Pivot, T),
  case Order(H, Pivot) of
    true  -> {[H|Less], Greater}; 
    false -> {[H|Less], Greater} %bug2 
    % false -> {Less, [H|Greater]} %OK
  end.
		
% quicksort_unit_test() ->
%   ?assertEqual(quicksort(fun leq/2, []), []), 
%   ?assertEqual(quicksort(fun leq/2, [1]), [1]),
%   ?assertEqual(quicksort(fun leq/2, [7,1]), [1,7]),
%   ?assertNotEqual(quicksort(fun leq/2, [7,8,1]), []).
%   %?assertEqual(quicksort(fun leq/2, [7,8,1]), [1,7,8] ).
  
% % quicksort_proper_complete() ->
% %   ?FORALL(L, list(integer()), 
% %     lists:sort( fun leq/2, L) =:= quicksort(fun leq/2, L) ).


% partition_proper_complete() -> 
%    ?FORALL({L,P}, {list(integer()), integer()}, 
%       partition(fun leq/2, P, L ) =:= lists:partition(fun(E) -> leq(E,P) end, L)).

% quicksort_unitary_proper_complete() ->
%     ?FORALL(X, list(integer()), 
%       ?IMPLIES( length(X) =:= 1,
%           quicksort(fun leq/2, X) =:= X)).
    
% % isSorted/2 is a trusted function
% isSorted(_, []) ->
%   true;
% isSorted(_, [_]) ->
%   true;
% isSorted(Order, [A,B|R] ) ->
%   Order(A,B) and isSorted(Order, [B|R]).
    
% quicksort_sorted_proper() ->
%   ?FORALL(L, list(integer()), isSorted(fun leq/2, quicksort(fun leq/2, L)) ).
  
% quicksort_length_proper() ->
%   ?FORALL(L, list(integer()), length(L) =:= length(quicksort(fun leq/2, L)) ).
 
% partition_length_proper() ->
%   ?FORALL({L,Pivot}, {list(integer()), integer()}, 
%     begin
%       {Less,Greater} = partition(fun leq/2, Pivot, L),
%       length(L) =:= length(Less) + length(Greater)
%     end).
    
% partition_split_proper() -> 
%   ?FORALL({L,Pivot}, {list(integer()), integer()},
%     begin
%       {Less,Greater} = partition(fun leq/2, Pivot, L),
%       lists:all(fun(A) -> A =< Pivot end, Less) and lists:all(fun(A) -> Pivot =< A end, Greater)
%     end).

quicksort_unit_test() ->
   ?assertEqual(quicksort(fun leq/2, []), []).
  
quicksort_unitary_proper_complete() ->
     ?FORALL(X, list(integer()), 
       ?IMPLIES( length(X) =:= 1, quicksort(fun leq/2, X) =:= X)).
    
isSorted(_, []) -> true;
isSorted(_, [_]) -> true;
isSorted(Order, [A,B|R] ) -> 
   Order(A,B) and isSorted(Order, [B|R]).
    
quicksort_sorted_proper() ->
   ?FORALL(L, list(integer()), isSorted(fun leq/2, quicksort(fun leq/2, L)) ).
  
quicksort_length_proper() ->
   ?FORALL(L, list(integer()), length(L) =:= length(quicksort(fun leq/2, L)) ).
  
partition_empty_proper_complete() ->
   ?FORALL(X, integer(), partition(fun leq/2, X, [] ) =:= {[], []}).
   
partition_length_proper() ->
   ?FORALL({L,Pivot}, {list(integer()), integer()}, 
      begin
         {Less,Greater} = partition(fun leq/2, Pivot, L),
         length(L) =:= length(Less) + length(Greater)
      end).
    
partition_split_proper() -> 
   ?FORALL({L,Pivot}, {list(integer()), integer()},
      begin
         {Less,Greater} = partition(fun leq/2, Pivot, L),
         lists:all(fun(A) -> leq(A, Pivot) end, Less) and 
            lists:all(fun(A) -> not leq(A, Pivot) end, Greater)
      end).
