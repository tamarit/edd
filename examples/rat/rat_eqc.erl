-module(rat_eqc).
% -include_lib("eqc/include/eqc.hrl").
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

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

gcd_2_complete() ->
    ?FORALL( {X,Y}, { non_zero_nat(), non_zero_nat() },
             rat:gcd(X,Y) =:= rat:gcd(Y,X)).

% gcd_4() ->
%     ?FORALL( {X,Y}, { non_zero_nat(), non_zero_nat()},
%              trusted(rat:gcd(X,Y))).

% trusted(_) -> true.

% trusted_fun({rat,gcd,2}) -> true;
% trusted_fun(_) -> false.


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
