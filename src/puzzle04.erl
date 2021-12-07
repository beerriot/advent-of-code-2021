%% Puzzle:
%%
%% Bingo

-module(puzzle04).

-export([
         solveA/0,
         solveA/2,
         score_win/2
        ]).

-compile([export_all]).

solveA() ->
    {ok, Data} = file:read_file("puzzles/puzzle04-input.txt"),
    [CallString | BoardStrings] = string:split(Data, <<"\n">>, all),
    Call = parse_call(CallString),
    Boards = parse_boards(BoardStrings),
    solveA(Call, Boards).

parse_call(CallString) ->
    [ binary_to_integer(N) ||
        N <- string:split(CallString, <<",">>, all) ].

parse_boards([<<>>,Row1,Row2,Row3,Row4,Row5|Boards]) ->
    [[parse_row(Row1),
      parse_row(Row2),
      parse_row(Row3),
      parse_row(Row4),
      parse_row(Row5)]
    |parse_boards(Boards)];
parse_boards(_) ->
    [].

parse_row(Row) ->
    [ binary_to_integer(N) ||
        N <- string:split(Row, <<" ">>, all),
        N =/= <<>> ].

solveA([Next|Rest], Boards) ->
    NewBoards = call_number(Next, Boards),
    case lists:filter(fun winning_board/1, NewBoards) of
        [B|_] ->
            {Next, B};
        [] ->
            solveA(Rest, NewBoards)
    end.

call_number(N, Boards) ->
    [ [ [case E of
             N -> x;
             _ -> E
         end || E <- R ]
        || R <- B]
      || B <- Boards ].

winning_board(Board) ->
    lists:any(
      fun(Row) -> lists:all(fun(C) -> C == x end, Row) end,
      Board ++ columns(Board)).

columns(Board) ->
    [ [lists:nth(C, R) || R <- Board] ||
        C <- [1,2,3,4,5] ].

score_win(Call, Board) ->
    lists:sum(
      [lists:sum([ N || N <- Row, N =/= x]) || Row <- Board])
        * Call.
