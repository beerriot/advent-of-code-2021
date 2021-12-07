%% Puzzle: report the number of times the number on the next line of a
%% file is larger than the number on the line before it.

-module(puzzle01a).

-export([
         solve/0,
         solve/1
        ]).

solve() ->
    solve("puzzles/puzzle01a-input.txt").

solve(Filename) ->
    {ok, Data} = file:read_file(Filename),
    NumberStrings = string:split(Data, <<"\n">>, all),
    report(NumberStrings).

report([First|Rest]) ->
    report(Rest, binary_to_integer(First), 0);
report([]) ->
    0.

report([<<>>], Last, Count) ->
    %% file ends with a newline
    Count;
report([NextString|Rest], Last, Count) ->
    Next = binary_to_integer(NextString),
    report(Rest, Next,
           case Next > Last of
               true -> Count +1;
               false -> Count
           end);
report([], _, Count) ->
    Count.
