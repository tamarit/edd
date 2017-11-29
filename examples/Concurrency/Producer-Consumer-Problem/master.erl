%% Obtained from https://github.com/NachiketUN/Producer-Consumer-Problem
%% Modified to be terminating

-module(master).
-export([init/1, init/5]).

% benchmark function
init(N) ->
  init(N, N, N, 2, 2).

init(MaxSize, NP, NC, TimesP, TimesC) ->
  %% Assume that (NP * TimesP) = (NC * TimesC)
  Buffer = spawn(fun() -> buffer:init(MaxSize) end),
  start_prod(1, NP, TimesP, Buffer),
  start_cons(1, NC, TimesC, Buffer).

start_prod(Prod_Id, Num_Prod, TimesP, Buffer) when Prod_Id =< Num_Prod ->
  spawn(fun() -> producer:init(Buffer, Prod_Id, TimesP) end),
  start_prod(Prod_Id + 1, Num_Prod, TimesP, Buffer);
start_prod(_Prod_Id, _Num_Prod, _TimesP, _Buffer) ->
  ok.

start_cons(Cons_Id, Num_Cons, TimesC, Buffer) when Cons_Id =< Num_Cons ->
  spawn(fun() -> consumer:init(Buffer, Cons_Id, TimesC) end),
  start_cons(Cons_Id + 1, Num_Cons, TimesC, Buffer);
start_cons(_Cons_Id, _Num_Prod, _TimesP, _Buffer) ->
  ok.
