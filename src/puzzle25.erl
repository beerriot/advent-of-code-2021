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

move_east([Line|Field]) ->
    [move_line_east(Line)|move_east(Field)];
move_east([]) ->
    [].

move_line_east([$.|Rest]) ->
    move_line_east(Rest, true, []);
move_line_east([$>,$.|Rest]) ->
    move_line_east(Rest, true, [$>]);
move_line_east(Line) ->
    move_line_east(Line, false, []).

move_line_east([$>,$.|Rest], MoveLast, RevLine) ->
    move_line_east(Rest, MoveLast, [$>,$.|RevLine]);
move_line_east([$>], true, Revline) ->
    [$>|lists:reverse([$.|Revline])];
move_line_east([C|Rest], MoveLast, RevLine) ->
    move_line_east(Rest, MoveLast, [C|RevLine]);
move_line_east([], true, RevLine) ->
    [$.|lists:reverse(RevLine)];
move_line_east([], false, RevLine) ->
    lists:reverse(RevLine).

move_south([First,Second|_]=Lines) ->
    move_south(Lines, First, Second, []).

move_south([Prev|[This,Next|_]=Rest], First, Second, RevLines) ->
    move_south(Rest, First, Second, [move_south(Prev, This, Next)|RevLines]);
move_south([NTL,Last], First, Second, RevLines) ->
    [move_south(Last, First, Second)
    |lists:reverse([move_south(NTL, Last, First)|RevLines])].

move_south([$v|RestP], [$.|RestT], [_|RestN]) ->
    [$v|move_south(RestP, RestT, RestN)];
move_south([_|RestP], [$v|RestT], [$.|RestN]) ->
    [$.|move_south(RestP, RestT, RestN)];
move_south([_|RestP], [T|RestT], [_|RestN]) ->
    [T|move_south(RestP, RestT, RestN)];
move_south([], [], []) ->
    [].
