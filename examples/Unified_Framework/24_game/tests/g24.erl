-module(g24).
-compile(export_all).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
 
 
test(Digts,Exp) ->
    io:format("Your digits\t~w~n", [Digts]),
    io:format("Your expression\t~p~n", [Exp]),
    read_eval(Digts,Exp).
 
main() ->
    random:seed(now()),
    io:format("24 Game~n"),
    play().
 
play() ->
    io:format("Generating 4 digits...~n"),
    Digts = [random:uniform(X) || X <- [9,9,9,9]],
    io:format("Your digits\t~w~n", [Digts]),
    Exp = string:strip(io:get_line(standard_io, "Your expression: "), both, $\n),
    read_eval(Digts,Exp),
    play().
 
read_eval(Digits,Exp) ->
    case {correct_nums(Exp, Digits), eval1(Exp)} of
        {ok, X} when X == 24 -> io:format("You Win!~n"), {win, X} ;
        {ok, X} -> io:format("You Lose with ~p!~n",[X]), {lose, X};
        {List, _} -> io:format("The following numbers are wrong: ~p~n", [List]), 
                     {invalid_chars, List}
    end.
 
correct_nums(Exp, Digits) ->
    case re:run(Exp, "([0-9]+)", [global, {capture, all_but_first, list}]) of
        nomatch ->
            "No number entered";
        {match, IntLs} ->
            case [X || [X] <- IntLs, not lists:member(list_to_integer(X), Digits)] of
                %[] -> ok; % CORRECT
                [] -> []; % WRONG
                L -> L
            end
    end.
 
eval1(Exp) ->
    {X, _} = eval2(re:replace(Exp, "\\s", "", [{return, list},global]),
                  0),
    X.
 
eval2([], Val) ->
    {Val,[]};
eval2([$(|Rest], Val) ->
    {NewVal, Exp} = eval2(Rest, Val),
    eval2(Exp, NewVal);
eval2([$)|Rest], Val) ->
    {Val, Rest};
eval2([$[|Rest], Val) ->
    {NewVal, Exp} = eval2(Rest, Val),
    eval2(Exp, NewVal);
eval2([$]|Rest], Val) ->
    {Val, Rest};
eval2([$+|Rest], Val) ->
    {NewOperand, Exp} = eval2(Rest, 0),
    eval2(Exp, Val + NewOperand);
eval2([$-|Rest], Val) ->
    {NewOperand, Exp} = eval2(Rest, 0),
    eval2(Exp, Val - NewOperand);
eval2([$*|Rest], Val) ->
    {NewOperand, Exp} = eval2(Rest, 0),
    eval2(Exp, Val * NewOperand);
eval2([$/|Rest], Val) ->
    {NewOperand, Exp} = eval2(Rest, 0),
    eval2(Exp, Val / NewOperand);
eval2([X|Rest], 0) when X >= $1, X =< $9 ->
    eval2(Rest, X-$0).
    
    
%% Tests %%
eval1_proper_complete() ->
  ?FORALL(ExprStr, string(), 
   begin
     Expr = lists:concat([ExprStr, "."]),
     {ok, Tokens, _} = erl_scan:string(Expr),
     {ok, Parsed} = erl_parse:parse_exprs(Tokens), 
     {value, Result, _} = erl_eval:exprs(Parsed, []),
     eval1(ExprStr) =:= Result
   end).

