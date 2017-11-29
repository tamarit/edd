-module(producer).
-export([init/3]).

-define(WAITING_PRODUCER, 10).

init(Buffer, Prod_Id, TimesP) ->
  io:format("Producer number ~w created~n",[Prod_Id]),
  listen(Buffer, Prod_Id, TimesP).

listen(Buffer, Prod_Id, TimesP) when TimesP > 0 ->
  timer:sleep(rand:uniform(5)*?WAITING_PRODUCER),
  %Item = spawn(fun() -> item:init() end),
  Item = {item, Prod_Id, TimesP}, % Generates Item
  Buffer ! {available, self(), Item, Prod_Id},
  receive
    full ->
      io:format("Producer ~w tried to insert item. But buffer is full.~n", [Prod_Id]),
      listen(Buffer, Prod_Id, TimesP);
    putItem ->
      %listen(Buffer, Prod_Id, TimesP - 1)  %% OK
      listen(Buffer, Prod_Id, TimesP - 2)  %% BUG
  end;
listen(_Buffer, Prod_Id, _TimesC) ->
  io:format("Producer number ~w finished~n",[Prod_Id]).
