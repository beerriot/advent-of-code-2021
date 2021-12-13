%% Puzzle:
%%
%% First half: report the number of times the number on the next line
%% of a file is larger than the number on the line before it.
%%
%% Second half: similar, but use a sliding window instead.
%%  -> solution to first half is now using a window size of 1
%%
%% https://adventofcode.com/2021/day/1

-module(puzzle01).

-export([
         solveA/0,
         solveB/0,
         count_increases/2
        ]).

solveA() ->
    count_increases(1, load_data()).

solveB() ->
    count_increases(3, load_data()).

load_data() ->
    {ok, Data} = file:read_file("puzzles/puzzle01-input.txt"),
    [ binary_to_integer(N) || N <- string:split(Data, <<"\n">>, all),
                              N =/= <<>> ].

count_increases(WindowLength, Numbers) ->
    ComparisonStart = lists:nthtail(WindowLength, Numbers),
    element(2, lists:foldl(
                 fun(New, {[Old|Rest], Count}) when New > Old ->
                         {Rest, Count + 1};
                    (_, {[_|Rest], Count}) ->
                         {Rest, Count}
                 end,
                 {Numbers, 0},
                 ComparisonStart)).
