-module(item).
-export([init/0]).

init() ->
  listen(0).

listen(Id) ->
  receive
    {assignId, ItemId} ->
      listen(ItemId);
    {From, getId} ->
      From ! {itemid, Id},
      listen(Id)
  end.
