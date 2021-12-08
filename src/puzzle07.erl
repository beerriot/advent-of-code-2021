%% Puzzle:
%%
%% Crab alignment

-module(puzzle07).

-export([
         solveA/0,
         find_best_position/1,
         median/1,
         cost_to_move/2
        ]).

solveA() ->
    find_best_position(load_crabs()).

load_crabs() ->
    {ok, Data} = file:read_file("puzzles/puzzle07-input.txt"),
    [OneLine|_] = string:split(Data, <<"\n">>),
    [ binary_to_integer(F) ||
        F <- string:split(OneLine, <<",">>, all) ].

find_best_position(Crabs) ->
    case median(Crabs) of
        [Median] ->
            {Median, cost_to_move(Median, Crabs)};
        [Median, Median] ->
            {Median, cost_to_move(Median, Crabs)};
        [A, B] ->
            case {cost_to_move(A, Crabs), cost_to_move(B, Crabs)} of
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

cost_to_move(Position, Crabs) ->
    lists:sum([abs(Position-C) || C <- Crabs]).
