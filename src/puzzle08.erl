%% Puzzle:
%%
%% Seven-segment displays

-module(puzzle08).

-export([
         solveA/0,
         parse_display/1,
         count_1478_outputs/1
        ]).

solveA() ->
    count_1478_outputs(load_displays()).

load_displays() ->
    {ok, Data} = file:read_file("puzzles/puzzle08-input.txt"),
    [ parse_display(D) || D <- string:split(Data, <<"\n">>, all),
                          D =/= <<>> ].

parse_display(D) ->
    {Patterns, [<<"|">>|Output]} =
        lists:splitwith(fun(C) -> C =/= <<"|">> end,
                        string:split(D, <<" ">>, all)),
    {Patterns, Output}.

count_1478_outputs(Displays) ->
    lists:sum([ length([ O || O <- Output, is_1478(O) ])
                || {_, Output} <- Displays ]).

is_1478(O) ->
    case size(O) of
        2 -> true; % 1
        4 -> true; % 4
        3 -> true; % 7
        7 -> true; % 8
        _ -> false
    end.
