%%%%%%%%%%%%%%%%%%%%%%%%%
% Enrique Martin-Martin %
% emartinm@ucm.es   %
%%%%%%%%%%%%%%%%%%%%%%%%%

% Stock Management
%
% The representation of the items in the stock are tuples
% {item, Item_Name, Quantity}
% The stock is represented as a list of items without repetitions of the same item
% Orders are also represented as lists of items

-module(stock).
-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").

% It should return that we need to buy 2 packs of rice and 3 bottles of water
% (i.e., [{item, rice, 2},{item,water,3}]), however it incorrectly returns
% [{item,water,3},{item,rice,7}]
test() ->
  check_orders( [[{item, water, 3},{item, rice, 3}],[{item, rice, 4}]], 
  [{item, rice, 5}, {item, bajoqueta, 8}]).


% Given one Order and a Stock, returns the list of items we have to buy to
% answer the order
% If we have enough items in the stock, returns []
check_order( Order, Stock ) ->
  lists:zf( fun(X) -> check_item(X,Stock) end, Order ).
  

% Function to use with lists:zf/2 to map and filter that checks the state of an
% item in the stock
% If there are enough items in the stock, returns false
% If there aren't enough items in the stock (or there are no items of that kind
% at all) returns {true, Item}, where item contains the quantity needed 
check_item( Needed = {item, Name, Q1}, Stock ) ->
  %ItemStock = lists:keyfind(Name, 2, Stock), %RIGHT
  ItemStock = lists:keyfind(Name, 1, Stock), %WRONG
  case ItemStock of  
    {item, Name, Q2} ->
      if Q1 > Q2 -> {true, {item, Name, Q1 - Q2}};
         true -> false
      end;
    false -> {true, Needed}
  end.


% Similar to check_order/2 but considering a list of orders
check_orders( List_of_orders, Stock ) ->
  Flat_orders = lists:flatten( List_of_orders ),
  Unique_orders = unify_orders( Flat_orders ),
  check_order( Unique_orders, Stock ).
  
  
% Unify a list of orders in one order, adding the needed quantities
unify_orders( [] ) -> [];
unify_orders( [{item,Name,Quantity}|R] ) ->
  {Item_orders, Other_orders} = lists:partition( fun({item, ItemName, _}) -> ItemName == Name end, R ),
  Total_quantity = lists:foldr( fun({item,_,N}, Acc) -> N + Acc end, 0, Item_orders),
  Unif_rest = unify_orders( Other_orders ),
  [ {item, Name, Total_quantity + Quantity} | Unif_rest ].



%% Tests %%
check_item_test() ->
	?assertEqual(check_item({item, rice, 7}, 
	                        [{item, rice, 5}, {item, bajoqueta, 8}]), 
	             {true, {item, rice, 2}}).

