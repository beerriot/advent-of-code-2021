%% Puzzle:
%%
%% parsing
%% https://adventofcode.com/2021/day/10
%%
%% explanation:
%% https://blog.beerriot.com/2021/12/20/advent-of-code-day-10/

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

solveB() ->
    middle_score(score_closings(load_lines())).

middle_score(Scores) ->
    lists:nth((length(Scores) div 2) + 1,
              lists:sort(Scores)).

score_closings(Lines) ->
    lists:filtermap(fun score_closing/1, Lines).

score_closing(L) ->
    score_closing(L, []).

score_closing(<<>>, []) ->
    false; % valid
score_closing(<<>>, Acc) ->
    {true, lists:foldl(fun(C, Score) ->
                               Score*5 + case C of
                                             $( -> 1;
                                             $[ -> 2;
                                             ${ -> 3;
                                             $< -> 4
                                         end
                       end,
                       0,
                       Acc)};
score_closing(<<$(, Rest/binary>>, Acc) ->
    score_closing(Rest, [$(|Acc]);
score_closing(<<$), Rest/binary>>, [$(|Acc]) ->
    score_closing(Rest, Acc);
score_closing(<<$), _/binary>>, [_|_]) ->
    false; % corrupt
score_closing(<<$[, Rest/binary>>, Acc) ->
    score_closing(Rest, [$[|Acc]);
score_closing(<<$], Rest/binary>>, [$[|Acc]) ->
    score_closing(Rest, Acc);
score_closing(<<$], _/binary>>, [_|_]) ->
    false;
score_closing(<<${, Rest/binary>>, Acc) ->
    score_closing(Rest, [${|Acc]);
score_closing(<<$}, Rest/binary>>, [${|Acc]) ->
    score_closing(Rest, Acc);
score_closing(<<$}, _/binary>>, [_|_]) ->
    false;
score_closing(<<$<, Rest/binary>>, Acc) ->
    score_closing(Rest, [$<|Acc]);
score_closing(<<$>, Rest/binary>>, [$<|Acc]) ->
    score_closing(Rest, Acc);
score_closing(<<$>, _/binary>>, [_|_]) ->
    false;
score_closing(<<_, _/binary>>, []) ->
    false.
