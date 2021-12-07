%% Puzzle:
%%
%% First half: Find most common binary digit in each position. Binary
%% number composed of most common is "gamma", inverse is "epsilon.

-module(puzzle03).

-export([
         solveA/0,
         solveA/1
        ]).

solveA() ->
    {ok, Data} = file:read_file("puzzles/puzzle03-input.txt"),
    Numbers = string:split(Data, <<"\n">>, all),
    solveA(Numbers).

solveA(Numbers) ->
    %% Total is theoretically length(Numbers), but that relies on file
    %% split doing the right thing with ending newlines, so count
    %% explicitly instead.
    {Total, Counts} = count_digits(Numbers),
    Gamma = make_gamma(Total, Counts),
    %% We can't just `bnot` because that will flip bits above our
    %% highest
    Epsilon = Gamma bxor (trunc(math:pow(2, length(Counts))) - 1),
    {Gamma, Epsilon}.

count_digits([First|_]=Numbers) ->
    Init = lists:duplicate(size(First), 0),
    count_digits(Numbers, 0, Init).

count_digits([<<>>], Total, Init) ->
    %% trailing newline
    {Total, Init};
count_digits([Number|Rest], Total, Counts) ->
    NewCounts = [ A+B ||
                    {A, B} <- lists:zip(Counts,
                                        [ C - $0 || <<C>> <= Number])],
    count_digits(Rest, Total+1, NewCounts);
count_digits([], Total, Init) ->
    %% no trailing newline
    {Total, Init}.

make_gamma(Total, Counts) ->
    Threshold = Total / 2,
    lists:foldl(fun(C, Acc) ->
                        (Acc bsl 1) + (case C > Threshold of
                                           true -> 1;
                                           false -> 0
                                       end)
                end, 0, Counts).
