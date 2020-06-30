-module(rfib_ok).
-compile([export_all]).

main(N) ->
	nfib(N).

nfib(N) when N =< 1 -> 1;
nfib(N) when N > 1 -> nfib(N-1) + nfib(N-2).
