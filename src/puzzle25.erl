%% Puzzle:
%%
%% Sea cucumbers
%% https://adventofcode.com/2021/day/25

-module(puzzle25).

-compile([export_all]).

load_file() ->
    {ok, Data} = file:read_file("puzzles/puzzle25-input.txt"),
    [ binary_to_list(L) || L <- string:split(Data, <<"\n">>, all),
                           L =/= <<>> ].

move_east(Field) ->
    lists:mapfoldl(fun move_line_east/2, 0, Field).

move_line_east([$.|Rest], MoveCount) ->
    move_line_east(Rest, true, [], MoveCount);
move_line_east([$>,$.|Rest], MoveCount) ->
    move_line_east(Rest, false, [$>,$.], MoveCount);
move_line_east(Line, MoveCount) ->
    move_line_east(Line, false, [], MoveCount).

move_line_east([$>,$.|Rest], MoveLast, RevLine, MoveCount) ->
    move_line_east(Rest, MoveLast, [$>,$.|RevLine], MoveCount+1);
move_line_east([$>], true, Revline, MoveCount) ->
    {[$>|lists:reverse([$.|Revline])], MoveCount+1};
move_line_east([C|Rest], MoveLast, RevLine, MoveCount) ->
    move_line_east(Rest, MoveLast, [C|RevLine], MoveCount);
move_line_east([], true, RevLine, MoveCount) ->
    {[$.|lists:reverse(RevLine)], MoveCount};
move_line_east([], false, RevLine, MoveCount) ->
    {lists:reverse(RevLine), MoveCount}.

move_south([First,Second|_]=Lines) ->
    lists:mapfoldl(
      fun({Line, Count}, Acc) -> {Line, Count+Acc} end,
      0,
      move_south(Lines, First, Second, [])).

move_south([Prev|[This,Next|_]=Rest], First, Second, RevLines) ->
    move_south(Rest, First, Second,
               [move_south(Prev, This, Next, [], 0)|RevLines]);
move_south([NTL,Last], First, Second, RevLines) ->
    [move_south(Last, First, Second, [], 0)
    |lists:reverse([move_south(NTL, Last, First, [], 0)|RevLines])].

move_south([$v|RestP], [$.|RestT], [_|RestN], Acc, Count) ->
    %% only count cucumbers moving out, not in
    move_south(RestP, RestT, RestN, [$v|Acc], Count);
move_south([_|RestP], [$v|RestT], [$.|RestN], Acc, Count) ->
    move_south(RestP, RestT, RestN, [$.|Acc], Count+1);
move_south([_|RestP], [T|RestT], [_|RestN], Acc, Count) ->
    move_south(RestP, RestT, RestN, [T|Acc], Count);
move_south([], [], [], Acc, Count) ->
    {lists:reverse(Acc), Count}.

move(Cucumbers) ->
    {East, EastCount} = move_east(Cucumbers),
    {South, SouthCount} = move_south(East),
    {South, SouthCount+EastCount}.

move(Cucumbers, 0) ->
    Cucumbers;
move(Cucumbers, N) ->
    {NewCukes, _} = move(Cucumbers),
    move(NewCukes, N-1).

moves_to_halt(Cucumbers) ->
    moves_to_halt(Cucumbers, 1).

moves_to_halt(Cucumbers, Step) ->
    case move(Cucumbers) of
        {_, 0} ->
            Step;
        {NewCukes, _} ->
            moves_to_halt(NewCukes, Step+1)
    end.
