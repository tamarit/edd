-module(rle).
-compile([export_all]).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

encode(S) ->
    doEncode(string:substr(S, 2), string:substr(S, 1, 1), 1, []).
 
doEncode([], CurrChar, Count, R) ->
    R ++ integer_to_list(Count) ++ CurrChar;
doEncode(S, CurrChar, Count, R) ->
    NextChar = string:substr(S, 1, 1),
    if
        NextChar == CurrChar ->
            % doEncode(string:substr(S, 2), CurrChar, Count + 1, R); % CORRECT
            doEncode(string:substr(S, 3), CurrChar, Count + 1, R);   % WRONG
        true ->
            doEncode(string:substr(S, 2), NextChar, 1, R ++ integer_to_list(Count) ++ CurrChar)
    end.
 
decode(S) ->
    doDecode(string:substr(S, 2), string:substr(S, 1, 1), []).
 
doDecode([], _, R) ->
    R;
doDecode(S, CurrString, R) ->
    NextChar = string:substr(S, 1, 1),
    IsInt = erlang:is_integer(catch(erlang:list_to_integer(NextChar))),
    if
        IsInt ->
            doDecode(string:substr(S, 2), CurrString ++ NextChar, R);
        true ->
            doDecode(string:substr(S, 2), [],
                R ++ string:copies(NextChar, list_to_integer(CurrString)))
    end.
 
test() ->
    PreEncoded =
        "WWWWWWWBWWWWWBBBWWWWWWWBWWWWW",
    Expected = "7W1B5W3B7W1B5W",
    (encode(PreEncoded) =:= Expected)
        and (decode(Expected) =:= PreEncoded) 
        and(decode(encode(PreEncoded)) =:= PreEncoded).
    %encode(PreEncoded).


%% Tests %%
doEnconde_test() ->
    ?assertNotEqual(doEncode([87, 87],
         [87],
         2,
         [52, 87, 49, 66, 51, 87, 50, 66, 52, 87, 49, 66]), [52, 87, 49, 66, 51, 87, 50, 66, 52, 87, 49, 66, 51, 87]),
	?assertNotEqual(doEncode([87, 87, 87, 87],
         [87],
         1,
         [52, 87, 49, 66, 51, 87, 50, 66, 52, 87, 49, 66]), [52, 87, 49, 66, 51, 87, 50, 66, 52, 87, 49, 66, 51, 87]).


