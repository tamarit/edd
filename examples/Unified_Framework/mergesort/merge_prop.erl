-module(merge_prop).
-export([mergesort/2, comp/2]).

% Calls to the declarative debugger:
% > edd:dd("merge:mergesort([b,a], fun merge:comp/2)").
% > edd:dd("merge:mergesort([o,h,i,o], fun merge:comp/2)").

mergesort([], _Comp) -> [];
mergesort([X], _Comp) -> [X];
mergesort(L, Comp) ->
    Half = length(L) div 2,
    L1 = take(Half, L),
    L2 = last(length(L) - Half, L),
    LOrd1 = mergesort(L1, Comp),
    LOrd2 = mergesort(L2, Comp),
    merge(LOrd1, LOrd2, Comp).

merge([], [], _Comp) ->
    [];
merge([], S2, _Comp) ->
    S2;
merge(S1, [], _Comp) ->
    S1;
merge([H1 | T1], [H2 | T2], Comp)  ->
        case Comp(H1,H2) of 
%            false -> [H2 | merge([H1 | T1], T2, Comp)]; % Correct
            false -> [H1 | merge([H2 | T1], T2, Comp)];  % Incorrect
            true ->  [H1 | merge(T1, [H2 | T2], Comp)]
        end.


comp(X,Y) when is_atom(X) and is_atom(Y) -> X < Y.


take(0,_) -> [];
take(1,[H|_])->[H];
take(_,[])->[];
take(N,[H|T])->[H | take(N-1, T)]. % Correct

last(N, List) ->
    lists:reverse(take(N, lists:reverse(List))).



% begin edd test

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

quicksort_sorted_proper() ->
   ?FORALL(L, list(integer()), isSorted(fun leq/2, quicksort(fun leq/2, L)) ).
%quicksort_sorted_proper() ->
%  ?FORALL(L, list(atom()), lists:sort(fun comp/2, L) =:= mergesort(L, comp/2)).

edd_test() ->
	?assertNotEqual(mergesort([c, b], fun comp/2), [c, b]),
	?assertNotEqual(merge([c], [b], fun comp/2), [c, b]),
	?assertEqual(merge([b], [], fun comp/2), [b]),
	?assertEqual(comp(c, b), false).

% end edd test

% begin edd trusted

edd_trusted() ->
	[].

% end edd trusted
