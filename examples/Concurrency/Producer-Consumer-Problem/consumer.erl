-module(consumer).
-export([init/3]).

-define(WAITING_CONSUMER, 10).

init(Buffer, Cons_Id, TimesC) ->
  io:format("Consumer number ~w created~n",[Cons_Id]),
  listen(Buffer, Cons_Id, TimesC).

listen(Buffer, Cons_Id, TimesC) when TimesC > 0 ->
  timer:sleep(rand:uniform(5)*?WAITING_CONSUMER),
  Buffer ! {occupied, self(), Cons_Id},
  receive
    empty ->
      io:format("Consumer ~w tried to remove item. But buffer is empty. ~n", [Cons_Id]),
      listen(Buffer, Cons_Id, TimesC);
    removeItem  ->
      %listen(Buffer, Cons_Id, TimesC - 1)  %% OK
      listen(Buffer, Cons_Id, TimesC - 2)  %% BUG
  end;
listen(_Buffer, Cons_Id, _TimesC) ->
  io:format("Consumer number ~w finished~n",[Cons_Id]).
