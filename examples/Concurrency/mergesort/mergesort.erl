-module(mergesort).
-export([mergesort/1, mergesort/2, mergesort/3, comp/2]).


mergesort(L) ->
    mergesort(L, fun ?MODULE:comp/2, none).

mergesort(L, Comp) ->
    mergesort(L, Comp, none).

mergesort([], _Comp, Parent) -> send_return([],Parent);
mergesort([X], _Comp, Parent) -> send_return([X],Parent);
mergesort(L, Comp, Parent) ->
    Half = length(L) div 2,
    L1 = take(Half, L),
    L2 = last(length(L) - Half, L),
    spawn(?MODULE, mergesort , [L1, Comp, self()]),
    spawn(?MODULE, mergesort , [L2, Comp, self()]),
    LOrd1 =
        receive 
            {result, LOrd1_} ->
                LOrd1_
        end,
    LOrd2 = 
        receive 
            {result, LOrd2_} ->
                LOrd2_
        end,
    send_return(merge(LOrd1, LOrd2, Comp), Parent).

send_return(Result,none) ->
    Result;
send_return(Result,Pid) ->
    Pid!{result, Result}.

merge([], [], _Comp) ->
    [];
merge([], S2, _Comp) ->
    S2;
merge(S1, [], _Comp) ->
    S1;
merge([H1 | T1], [H2 | T2], Comp)  ->
        case Comp(H1,H2) of 
            false -> [H2 | merge([H1 | T1], T2, Comp)];   % Correct
            %false -> [H1 | merge([H2 | T1], T2, Comp)];  % Incorrect
            true ->  [H1 | merge(T1, [H2 | T2], Comp)]
        end.


comp(X,Y) -> 
    X < Y.


take(0,_) -> [];
take(1,[H|_])->[H];
take(_,[])->[];
% take(N,[H|T])->[H | take(N-1, T)]. % Correct
take(N,[_|T])->[N | take(N-1, T)].   % Incorrect

last(N, List) ->
    lists:reverse(take(N, lists:reverse(List))).
