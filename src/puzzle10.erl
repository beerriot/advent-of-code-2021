%% Puzzle:
%%
%% parsing

-module(puzzle10).

-export([
         solveA/0
        ]).

-compile([export_all]).

solveA() ->
    score_corruptions(load_lines()).

load_lines() ->
    {ok, Data} = file:read_file("puzzles/puzzle10-input.txt"),
    string:split(Data, <<"\n">>, all).

score_corruptions(Lines) ->
    lists:sum([ score_corruption(L, []) || L <- Lines ]).

score_corruption(<<>>, _ ) ->
    0;
score_corruption(<<$(, Rest/binary>>, Acc) ->
    score_corruption(Rest, [$(|Acc]);
score_corruption(<<$), Rest/binary>>, [$(|Acc]) ->
    score_corruption(Rest, Acc);
score_corruption(<<$), _/binary>>, [_|_]) ->
    3;
score_corruption(<<$[, Rest/binary>>, Acc) ->
    score_corruption(Rest, [$[|Acc]);
score_corruption(<<$], Rest/binary>>, [$[|Acc]) ->
    score_corruption(Rest, Acc);
score_corruption(<<$], _/binary>>, [_|_]) ->
    57;
score_corruption(<<${, Rest/binary>>, Acc) ->
    score_corruption(Rest, [${|Acc]);
score_corruption(<<$}, Rest/binary>>, [${|Acc]) ->
    score_corruption(Rest, Acc);
score_corruption(<<$}, _/binary>>, [_|_]) ->
    1197;
score_corruption(<<$<, Rest/binary>>, Acc) ->
    score_corruption(Rest, [$<|Acc]);
score_corruption(<<$>, Rest/binary>>, [$<|Acc]) ->
    score_corruption(Rest, Acc);
score_corruption(<<$>, _/binary>>, [_|_]) ->
    25137;
score_corruption(<<_, _/binary>>, []) ->
    0.
