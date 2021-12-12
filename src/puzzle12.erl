%% Puzzle:
%%
%% cave network

-module(puzzle12).

-export([
         solveA/0
        ]).

-compile([export_all]).

solveA() ->
    count_paths(<<"start">>, load_network()).

load_network() ->
    {ok, Data} = file:read_file("puzzles/puzzle12-input.txt"),
    build_network([ string:split(Line, <<"-">>)
                    || Line <- string:split(Data, <<"\n">>, all),
                       Line =/= <<>> ]).

build_network(Connections) ->
    lists:foldl(fun([A,B], Net) ->
                        add_link(A, B, add_link(B, A, Net))
                end,
                [],
                Connections).

add_link(From, To, Net) ->
    case lists:keytake(From, 1, Net) of
        {value, {From, Links}, Rest} ->
            [{From, [To|Links]}|Rest];
        false ->
            [{From, [To]}|Net]
    end.

count_paths(Start, Network) ->
    case lists:keytake(Start, 1, Network) of
        {value, {Start, InitialLinks}, Rest} ->
            RealRest = case Start of
                           <<C, _/binary>> when C >= $A, C =< $Z ->
                               [{Start, InitialLinks}|Rest];
                           _ ->
                               Rest
                       end,
            lists:sum([ case L of
                            <<"end">> -> 1;
                            _ -> count_paths(L, RealRest)
                        end || L <- InitialLinks ]);
        false ->
            0
    end.
