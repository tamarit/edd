-module(caesar).
-compile(export_all).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
 
%% rot: rotate Char by Key places
rot(Char,Key) when (Char >= $A) and (Char =< $Z) or
                   (Char >= $a) and (Char =< $z) ->
  Offset = $A + Char band 32,
  N = Char - Offset,
  %Offset + (N + Key) rem 26; %RIGHT
  Offset + (N - Key) rem 26; %WRONG
rot(Char, _Key) ->
  Char.
 
%% key: normalize key.
key(Key) when Key < 0 ->
  26 + Key rem 26;
key(Key) when Key > 25 ->
  Key rem 26;
key(Key) ->
  Key.
 
main(PlainText, Key) ->
  Encode = key(Key),
  Decode = key(-Key),
 
  io:format("Plaintext ----> ~s~n", [PlainText]),
 
  CypherText = lists:map(fun(Char) -> rot(Char, Encode) end, PlainText),
  io:format("Cyphertext ---> ~s~n", [CypherText]),
 
  PlainText = lists:map(fun(Char) -> rot(Char, Decode) end, CypherText).
  
test() ->
   main("The five boxing wizards jump quickly", 3). 



%% Tests %%
prop_rot_lower1_subset() -> 
  ?FORALL({Char, Key}, {range($a, $z), integer()}, rot(Char,Key) >= $a).

