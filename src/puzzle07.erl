%% Puzzle:
%%
%% Crab alignment
%% https://adventofcode.com/2021/day/7
%%
%% explanation:
%% https://blog.beerriot.com/2021/12/17/advent-of-code-day-7/

-module(puzzle07).

-export([
         solveA/0,
         solveB/0,
         find_best_positionA/1,
         find_best_positionB/1,
         median/1,
         mean/1,
         cost_to_moveA/2,
         cost_to_moveB/2
        ]).

solveA() ->
    find_best_positionA(load_crabs()).

solveB() ->
    find_best_positionB(load_crabs()).

load_crabs() ->
    {ok, Data} = file:read_file("puzzles/puzzle07-input.txt"),
    [OneLine|_] = string:split(Data, <<"\n">>),
    [ binary_to_integer(F) ||
        F <- string:split(OneLine, <<",">>, all) ].

find_best_positionA(Crabs) ->
    case median(Crabs) of
        [Median] ->
            {Median, cost_to_moveA(Median, Crabs)};
        [Median, Median] ->
            {Median, cost_to_moveA(Median, Crabs)};
        [A, B] ->
            case {cost_to_moveA(A, Crabs), cost_to_moveA(B, Crabs)} of
                {CostA, CostB} when CostA < CostB ->
                    {A, CostA};
                {_, CostB} ->
                    {B, CostB}
            end
    end.

median(Crabs) ->
    case length(Crabs) rem 2 of
        1 ->
            [lists:nth(length(Crabs) div 2, lists:sort(Crabs))];
        0 ->
            [A,B|_] = lists:nthtail(length(Crabs) div 2 - 1,
                                    lists:sort(Crabs)),
            [A,B]
    end.

cost_to_moveA(Position, Crabs) ->
    lists:sum([abs(Position-C) || C <- Crabs]).

find_best_positionB(Crabs) ->
    case mean(Crabs) of
        Mean when trunc(Mean) == Mean ->
            {trunc(Mean), cost_to_moveB(trunc(Mean), Crabs)};
        Mean ->
            case {cost_to_moveB(trunc(Mean), Crabs),
                  cost_to_moveB(trunc(Mean)+1, Crabs)} of
                {CostA, CostB} when CostA < CostB ->
                    {trunc(Mean), CostA};
                {_, CostB} ->
                    {trunc(Mean)+1, CostB}
            end
    end.

mean(Crabs) ->
    lists:sum(Crabs) / length(Crabs).

cost_to_moveB(Position, Crabs) ->
    lists:sum([lists:sum(lists:seq(1, abs(Position-C))) || C <- Crabs]).
