-module(dutch_ok).
-compile([export_all]).
 
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
 
dutch(L) -> dutch([],[],[],L).
 
dutch(R, W, B, [])          -> R ++ W ++ B;
dutch(R, W, B, [red   | L]) -> dutch([red|R],  W,  B,  L);
dutch(R, W, B, [white | L]) -> dutch(R, [white|W], B,  L);
dutch(R, W, B, [blue  | L]) -> dutch(R, W,   [blue|B], L). %RIGHT

main() ->
   L = [white,red,white,white,red,white,red,blue,red,red,white,white,blue,blue,white,red,white,blue,blue,white],
   case is_dutch(L) of
     true  -> L;
     false -> OrderedL = dutch(L),
              OrderedL
   end.
