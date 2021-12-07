%% Puzzle:
%%
%% First half: report the number of times the number on the next line
%% of a file is larger than the number on the line before it.
%%
%% Second half: similar, but use a sliding window instead.
%%  -> solution to first half is now using a window size of 1

-module(puzzle01).

-export([
         solve/1,
         solve/2
        ]).

solve(Window) ->
    solve(Window, "puzzles/puzzle01-input.txt").

solve(Window, Filename) ->
    {ok, Data} = file:read_file(Filename),
    NumberStrings = string:split(Data, <<"\n">>, all),
    {StartWindow, Rest} = fill_window(Window, NumberStrings),
    report(Rest, StartWindow, 0).

fill_window(Size, Strings) ->
    {RevWindow, Rest} = lists:split(Size, Strings),
    {lists:reverse([ binary_to_integer(N) || N <- RevWindow ]), Rest}.


report([<<>>], _, Count) ->
    %% file ends with a newline
    Count;
report([NextString|Rest], Window, Count) ->
    Next = binary_to_integer(NextString),
    NewWindow = [Next | lists:droplast(Window)],
    report(Rest, NewWindow,
           case lists:sum(NewWindow) > lists:sum(Window) of
               true -> Count + 1;
               false -> Count
           end);
report([], _, Count) ->
    Count.
