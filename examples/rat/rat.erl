%%%-------------------------------------------------------------------
%%% @author wojtek
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. Mar 2015 11:34 AM
%%%-------------------------------------------------------------------
-module(rat).
-author("wojtek").

% cd("../rat/src"), c(rat), rat:gcd(2,3), edd:dd( "rat:gcd(2,3)", tree).

%% A rational number is a pair {L,M}, where L is an integer, and
%% M is a non zero natural number (positive integer).


%% API
-export(
   [
    gcd/2, lcm/2,
    rat/1, rat/2,
    add/2,
    minus/1, minus/2,
    mult/2,
    inverse/1,
    divide/2,
    ge/2,
    is_rational/1,
    to_float/1,
    to_int/1,
    floor/1, floor/2
   ]).


%% Greatest common divider of X and Y
% -spec gcd(X::pos_integer(),Y::pos_integer()) -> pos_integer().
gcd(X,Y) when (X < Y) ->
    gcd(Y,X);
gcd(X,Y) ->
    case (X rem Y) of
        0 -> Y;
        R ->
            gcd(Y,R)
    end.

%% Least common multiplier of X and Y
% -spec lcm(X::pos_integer(),Y::pos_integer()) -> pos_integer().
lcm(X,Y) ->
    G = gcd(X,Y),
    (X div G) * Y.

%% generates a `rat` number
% -spec rat(L::integer(),M::pos_integer()) -> rat().
rat(X,Y) ->
    rat({X,Y}).

%% normalizes [to] a rational number
% -spec rat(X::as_rat()) -> rat().
rat({0,_M}) when _M =/= 0 ->
    {0,1};
rat({L,M}) when L > 0, M =/= 0 ->
    G = gcd(L,M),
    {L div G, M div G};
rat({L,M}) when L < 0, M =/= 0 ->
    {Lr,Mr} = rat({-L,M}),
    {-Lr,Mr};
rat(I) when is_integer(I) ->
    rat(I,1);
rat(F) when is_float(F) ->
    case <<F/float>> of
        <<_:1,0:63>> -> rat(0);
        <<Sign:1, Exp:11, Mant:52/bitstring>> ->
            <<M:56,_:1>> = <<1:1,Mant:52/bitstring,0:4>>,
            E = Exp - 1023 - 54,
            S = 1 - 2 * Sign,
            case (E >= 0) of
                true -> rat(S * M  bsl E);
                _ -> rat(S * M, 2 bsl -E)
            end
    end.

% -spec add(X::as_rat(), Y::as_rat()) -> rat().
add({L1,M1},{L2,M2}) ->
    rat({L1 * M2 + L2 * M1, M1 * M2});
add(X,Y) ->
    add(rat(X),rat(Y)).

% -spec minus(X::as_rat()) -> rat().
minus({L,M}) ->
    {-L,M};
minus(X) ->
    minus(rat(X)).

% -spec minus(X::as_rat(), Y::as_rat()) -> rat().
minus(X,Y) ->
    add(X, minus(Y)).

% -spec mult(X::as_rat(), Y::as_rat()) -> rat().
mult({L1,M1},{L2,M2}) ->
    rat({L1*L2,M1*M2});
mult(X,Y) ->
    mult(rat(X),rat(Y)).

% -spec inverse(X::non_zero_rat()) -> rat().
inverse({L,M}) when L > 0 ->
    {M,L};
inverse({L,M}) when L < 0 ->
    {-M,-L}.

% -spec divide(X::as_rat(), Y::non_zero_rat()) -> rat().
divide(X,Y) ->
    mult(X,inverse(Y)).

% -spec ge(X::as_rat(), Y::as_rat()) -> boolean().
ge({L1,M1},{L2,M2}) ->
    L1 * M2 >= L2 * M1;
ge(X,Y) ->
    ge(rat(X),rat(Y)).

% -spec is_rational(any()) -> boolean().
is_rational(X={L,M}) when is_integer(L), is_integer(M), M > 0 ->
    X =:= rat(X);
is_rational(_) ->
    false.

% -spec to_float(X::rat()) -> float().
to_float({L,M}) ->
    L / M.

% -spec to_int(X::rat()) -> integer().
to_int({L,M}) ->
    L div M.

%% floor to `Precision`, eg:
%%   floor({1,3},{1,100})  -> {33,100}, i.e:  0.33
%%   floor({-1,3},{1,100}) -> {-17,50}, i.e: -0.34
%%   floor(2.711, {2,3})   -> {8,3}
% -spec floor(X::as_rat(), Prec::non_zero_rat()) -> rat().
floor(X = {L,_M},Prec) when L >= 0 ->
    % mult(to_int(divide(X,Prec)),Prec);
    mult(to_int(divide(X,Prec)),X);
floor(X = {_L,_M} ,Prec) ->
    E = floor(minus(X),Prec),
    case minus(E) of
        X -> 
            X;
        R ->
            minus(R, Prec) 
    end;
floor(X,Prec) ->
    floor(rat(X), Prec).

% -spec floor(X::as_rat()) -> rat().
floor(X) ->
    floor(X,{1,1}).

% begin edd trusted

edd_trusted() ->
	[{rat,mult,2}, {rat,rat,2}].

% end edd trusted
