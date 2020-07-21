-module(sieve).
-export([find_primes_below/1]).

-include_lib("eunit/include/eunit.hrl").
 
 
find_primes_below(N) ->
    NumList = lists:seq(1, N),
    determine_primes(NumList, 1, []).
 
%% Sieve of Eratosthenes algorithm
determine_primes(NumList, Index, Primes) ->
    case next_prime(NumList, Index+1, length(NumList)) of
	{Prime, PrimeIndex, NewNumList} ->
	    NewPrimes = lists:append(Primes, [Prime]),
	    determine_primes(NewNumList, PrimeIndex, NewPrimes);
	_ ->
	    %% All prime numbers have been calculated
	    Primes
    end.
 
next_prime(_, Index, Length) when Index > Length ->
	false;
next_prime(NumList, Index, Length) -> 
    case lists:nth(Index, NumList) of
		0 ->
		    %next_prime(NumList, Index+1, Length); % RIGHT
		    next_prime(NumList, Index+2, Length);  % WRONG
		Prime ->
		    NewNumList = lists:map(fun(A) ->
						   if A > Index andalso A rem Index == 0 ->  0;
						      true -> A
						   end
					   end, NumList),
		    {Prime, Index, NewNumList}
    end.


%% Tests %%
prime_test() ->
    ?assertEqual(find_primes_below(20), [2,3,5,7,11,13,17,19]),
	?assertEqual(next_prime([1,2,3,0,5,0,7,0,0,0,11,0,13,0,0,0,17,0,19,0], 18, 20), 19),	
	?assertEqual(next_prime([1,2,3,0,5,0,7,0,0,0,11,0,13,0,0,0,17,0,19,0], 20, 20), false).

