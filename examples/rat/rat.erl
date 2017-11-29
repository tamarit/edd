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

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

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
-compile(export_all).   

-export_type([as_rat/0,rat/0,pos_rat/0,non_zero_rat/0]).

-type rat() :: {integer(), pos_integer()}.
-type pos_rat() :: {pos_integer(),pos_integer()}.
-type non_zero_rat() :: {neg_integer()|pos_integer(),pos_integer()}.
-type as_rat() :: rat() | integer() | float().
   

%% Greatest common divider of X and Y
% -spec gcd(X::pos_integer(),Y::pos_integer()) -> pos_integer().
gcd(X,Y) when (X < Y) ->
    gcd(Y,X);
gcd(X,Y) ->
    case (X rem Y) of
        0 -> Y + 1; %BUG
        %0 -> Y; %OK
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
    
    
    
    
    
    
    
    
rat_test() ->
    ?assertEqual(rat:gcd(18,8), 2).

non_zero_nat() ->
    ?SUCHTHAT(N, nat(), N > 0).

rat() ->
    ?SUCHTHAT({L,M}, {largeint(),non_zero_nat()}, rat:is_rational({L,M})).

pos_rat() ->
    ?SUCHTHAT({L,M}, {non_zero_nat(),non_zero_nat()}, rat:is_rational({L,M})).

non_zero_rat() ->
    ?SUCHTHAT(R, rat(), R /= {0,1}).

gcd_1() ->
    ?FORALL( X, non_zero_nat(),
             rat:gcd(X,X+1) =:= 1).

gcd_2() ->
    ?FORALL( {X,Y}, { non_zero_nat(), non_zero_nat() },
             rat:gcd(X,Y) =:= rat:gcd(Y,X)).

gcd_2_complete() ->
    ?FORALL( {X,Y}, { non_zero_nat(), non_zero_nat() },
             rat:gcd(X,Y) =:= rat:gcd(Y,X)).

gcd_3() ->
    ?FORALL( {X,K}, {nat(),nat()},
             ?IMPLIES(X > K,
                      begin
                          Y = X * (K+1),
                          rat:gcd(X,Y) > K
                      end)).

lcm_1() ->
    ?FORALL( {X,Y}, {non_zero_nat(),non_zero_nat()},
             begin
                 rat:gcd(X,Y)*rat:lcm(X,Y) =:= X*Y
             end).

rat_1() ->
    ?FORALL( {X,Y}, {largeint(),non_zero_nat()},
             begin
                 rat:is_rational(rat:rat(X,Y))
             end).
rat_2() ->
    ?FORALL( X, largeint(),
             rat:to_int(rat:rat(X)) =:= X).

rat_3() ->
    ?FORALL( X, real(),
             begin
                 %% D  = (1.0 / (1 bsl 52)),
                 %% XX = rat:to_float(rat:rat(X)),
                 %% io:format("~p~n",[{X,XX,D}]),
                 %% io:format("~p~n",[{<<X/float>>,<<XX/float>>,D}]),
                 %% case (X > 0.0) of
                 %%     true ->
                 %%         (X * (1.0 - D) =< XX) 
                 %%             and
                 %%               (XX =< X * (1.0 + D));
                 %%     _ ->
                 %%         (X * (1.0 - D) >= XX) 
                 %%             and
                 %%               (XX >= X * (1.0 + D))
                 %% end,
                 X == rat:to_float(rat:rat(X))
             end).

minus_1() ->
    ?FORALL( R, rat(),
             begin
                 rat:minus(rat:minus(R)) =:= R
             end).
minus_2() ->
    ?FORALL( R, rat(),
             begin
                 rat:minus(R,R) =:= rat:rat(0)
             end).

add_1() ->
    ?FORALL( {X,Y}, {rat(),rat()},
             begin
                 rat:add(X,Y) =:= rat:add(Y,X)
             end).
add_2() ->
    ?FORALL( {X,Y,Z}, {rat(),rat(),rat()},
             begin
                 rat:add(rat:add(X,Y),Z) =:= 
                     rat:add(X,rat:add(Y,Z))
             end).

mult_1() ->
    ?FORALL( {X,Y}, {rat(),rat()},
             begin
                 rat:mult(X,Y) =:= rat:mult(Y,X)
             end).
mult_2() ->
    ?FORALL( {X,Y,Z}, {rat(),rat(),rat()},
             begin
                 rat:mult(rat:mult(X,Y),Z) =:=
                     rat:mult(X,rat:mult(Y,Z))
             end).
mult_3() ->
    ?FORALL( X, non_zero_rat(),
             rat:mult(X,rat:inverse(X)) =:= rat:rat(1,1)).

add_mult_1() ->
    ?FORALL( {X,Y,Z}, {rat(),rat(),rat()},
             begin
                 rat:mult(X,rat:add(Y,Z))
                     =:= 
                     rat:add(rat:mult(X,Y),rat:mult(X,Z))
             end).

ge_1() ->
    ?FORALL( X, rat(),
             rat:ge(rat:mult(X,X), {0,1})
           ).
ge_2() ->
    ?FORALL( {X,Y,Z}, {rat(),rat(),rat()},
             (not rat:ge(X,Y))
             or
               (not rat:ge(Y,Z))
             or
             (rat:ge(X,Z))).
ge_3() ->
    ?FORALL( {X,Y}, {real(),real()},
             ((not (X >= Y)) and (not rat:ge(X,Y)))
              or
             ((X >= Y) and (rat:ge(X,Y)))).

is_rational_1() ->
    ?FORALL({L,M} , rat(),
            ?IMPLIES( L > 0,
                      rat:is_rational({L,M}) =:= (rat:gcd(L,M) =:= 1)
                    )).

inverse_1() ->
    ?FORALL(X, non_zero_rat(),
            rat:inverse(rat:inverse(X)) =:= X).
inverse_2() ->
    ?FORALL(X, non_zero_rat(),
            case rat:mult(X,rat:inverse(X)) of
                {S,1} ->
                    S*S =:= 1;
                _ ->
                    false
            end
           ).

floor_1() ->
    ?FORALL({X,Prec} , {rat(), pos_rat()},
            begin
                Floor = rat:floor(X,Prec),
                rat:ge(X,Floor)
                    and
                    rat:ge(rat:add(X,Prec),X)
            end).
floor_2() ->
    ?FORALL({X,Prec} , {real(), pos_rat()},
            begin
                Floor = rat:floor(X,Prec),
                (X >= rat:to_float(Floor))
                    and
                    (rat:to_float(rat:add(Floor,Prec)) >= X)
            end).
floor_3() ->
    ?FORALL(R={_,M} , rat(),
            rat:floor(R,{1,M}) =:= R).
floor_4() ->
    ?FORALL(R , real(),
            ?IMPLIES( R > 0,
                      rat:to_int(rat:floor(R)) =:= erlang:trunc(R))).

all_test() ->
    [
        gcd_1,gcd_2,gcd_3,lcm_1,
        rat_1,rat_2,rat_3,
        minus_1,minus_2,
        add_1,add_2,
        inverse_1, inverse_2,
        mult_1,mult_2,mult_3,
        add_mult_1,
        ge_1, ge_2,ge_3,
        is_rational_1,
        floor_1, floor_2, floor_3, floor_4
    ].

run_all() ->
    [begin 
        io:format("Test: ~p\n", [F]),
        proper:quickcheck(
            % proper:numtests(10000, ?MODULE:F())
            ?MODULE:F()
            )
    end || F <- all_test()],
    ok.

eqc_test_() ->
    [ { atom_to_list(F),
        {timeout, 20,
         ?_assert(
            eqc:quickcheck(
              eqc:numtests(10000,
                           ?MODULE:F())))}} 
      || F <- all_test()].    
