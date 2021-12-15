%% Puzzle:
%%
%% Bingo
%%
%% https://adventofcode.com/2021/day/4
%%
%% explanation:
%% https://blog.beerriot.com/2021/12/15/advent-of-code-day-4/

-module(puzzle04).

-export([
         solveA/0,
         solveA/2,
         solveB/0,
         solveB/2,
         parse_input/1,
         score_win/2
        ]).

solveA() ->
    {Call, Boards} = parse_file("puzzles/puzzle04-input.txt"),
    solveA(Call, Boards).

solveB() ->
    {Call, Boards} = parse_file("puzzles/puzzle04-input.txt"),
    solveB(Call, Boards).

parse_file(Filename) ->
    {ok, Data} = file:read_file(Filename),
    parse_input(Data).

parse_input(Data) ->
    [CallString | BoardStrings] = string:split(Data, <<"\n">>, all),
    Call = parse_call(CallString),
    Boards = parse_boards(BoardStrings),
    {Call, Boards}.

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

solveB([Next|Rest], Boards) ->
    NewBoards = call_number(Next, Boards),
    case lists:partition(fun winning_board/1, NewBoards) of
        {[W], []} ->
            {Next, W};
        {_, NonWinners} ->
            solveB(Rest, NonWinners)
    end.

call_number(N, Boards) ->
    [ [ [case E of N -> x; _ -> E end
         || E <- R ]
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
