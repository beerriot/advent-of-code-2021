%% Puzzle:
%%
%% transparent paper folding
%% https://adventofcode.com/2021/day/17
%%
%% Initial A solution: no code needed

%%  - highest Y velocity for target below start is abs(lower_end)-1,
%%    because it will trace same steps up and down, and then makes one
%%    more step, which needs to hit bottom end
%%
%%  - highest Y reached during that point is (Y * (Y+1))/2

-module(puzzle17).

-export([
         solveA/0
        ]).

solveA() -> see_comment.
