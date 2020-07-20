-module(dutch).
%-export([main/1, main/0]).
-compile(export_all).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
 
ball(red)   -> 1;
ball(white) -> 2;
ball(blue)  -> 3.
 
random_ball() -> lists:nth(random:uniform(3), [red, white, blue]).
 
random_balls(N)   -> random_balls(N,[]).
random_balls(0,L) -> L;
random_balls(N,L) when N > 0 ->
  B = random_ball(),
  random_balls(N-1, [B|L]).
 
is_dutch([])        -> true;
is_dutch([_])       -> true;
is_dutch([B|[H|L]]) -> (ball(B) < ball(H)) and is_dutch([H|L]);
is_dutch(_)         -> false.
 
dutch_list(L) -> dutch([],[],[],L).
 
dutch(R, W, B, [])          -> R ++ W ++ B;
dutch(R, W, B, [red   | L]) -> dutch([red|R],  W,  B,  L);
dutch(R, W, B, [white | L]) -> dutch(R, [white|W], B,  L);
%dutch(R, W, B, [blue  | L]) -> dutch(R, W, [blue|B], L). %RIGHT
dutch(R, W, B, [blue  | L]) -> dutch(R, W,   B, L).         %WRONG

main(N) ->
   L = random_balls(N),
   case is_dutch(L) of
     true  -> io:format("The random sequence ~p is already in the order of the Dutch flag!~n", [L]);
     false -> io:format("The starting random sequence is ~p;~nThe ordered sequence is ~p.~n", [L, dutch_list(L)])
   end.
   
main() ->
   L = [white,red,white,white,red,white,red,blue,red,red,white,white,blue,blue,white,red,white,blue,blue,white],
   case is_dutch(L) of
     true  -> L;
     false -> OrderedL = dutch_list(L),
              OrderedL
   end.


%% Tests
comp_color(red, red)     -> true;
comp_color(red, white)   -> true;
comp_color(red, blue)    -> true;
comp_color(white, red)   -> false;
comp_color(white, white) -> true;
comp_color(white, blue)  -> true;
comp_color(blue, red)    -> false;
comp_color(blue, white)  -> false;
comp_color(blue, blue)   -> true.



red() ->
  red.
  
blue() ->
  blue.
  
white() ->
  white.  
  
rbw_list() ->
    list(frequency([{33, red}, {33, blue}, {34, white}])).
  
prop_dutch_complete() ->
   ?FORALL({R,W,B,Rest},
           {list(red()), list(white()), list(blue()), rbw_list()},
           dutch(R,W,B,Rest) =:= lists:sort(fun comp_color/2, R++W++B++Rest)).
           

