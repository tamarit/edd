%edd:dd("rfib:main(10)"). 
-module(rfib).

-export([main/1]).

%---------------------------------------------------------
%--       WARNING: THIS PROGRAM CONTAINS A BUG!!!       --
%--                                                     --
%--  This program belongs to the faulty nofib library   --
%--      and contains a bug to benchmark debuggers      --
%--                                                     --
%---------------------------------------------------------
%--                                                     --
%--  "The faulty nofib library" is a  collection of     --
%--  Haskell programs from the 'nofib' benchmark suite  --
%--                                                     --
%--  Faults are always marked with a comment: "BUG"     --
%--  The commented correct line appears after the       --
%--  faulty line marked with "CORRECT"                  --
%--                                                     --
%--  We welcome any comment or improvement about        --
%--  bugs. You can send them to:                        --
%--        Josep Silva (jsilva@dsic.upv.es)             --
%--                                                     --
%---------------------------------------------------------
%--                                                     --
%--  There are three kinds of bugs depending on their   --
%--  consequences:                                      --
%--  1) Bugs that produce an incorrect result           --
%--  2) Bugs that produce non-termination               --
%--  3) Bugs that produce an exception (e.g. div by 0)  --
%--                                                     --
%--  This program contains a bug of tipe 1              --
%---------------------------------------------------------
%
%-- !!! the ultra-notorious "nfib 30" does w/ Floats
%--
%module Main (main) where
%import System
%
%main = do
%	[arg] <- getArgs
%	print $ nfib $ read arg
%

main(N) ->
	nfib(N).

%nfib :: Double -> Double
%-- BUG: The following line contains a bug:
%nfib n = if n < 1 then 1 else nfib (n-1) + nfib (n-2)
%-- CORRECT -- nfib n = if n <= 1 then 1 else nfib (n-1) + nfib (n-2)
%

nfib(N) when N < 1 -> 1; %WRONG
%nfib(N) when N =< 1 -> 1; %RIGHT
nfib(N) when N >= 1 -> nfib(N-1) + nfib(N-2).
