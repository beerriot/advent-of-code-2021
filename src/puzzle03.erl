%% Puzzle:
%%
%% First half: Find most common binary digit in each position. Binary
%% number composed of most common is "gamma", inverse is "epsilon.
%%
%% https://adventofcode.com/2021/day/3

-module(puzzle03).

-export([
         solveA/0,
         solveA/1,
         count_digits/1,
         make_gamma/2,
         solveB/0,
         solveB/1
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
    Threshold = Total div 2,
    <<Int:(length(Counts))/integer>> =
        << <<(C div Threshold):1>> || C <- Counts >>,
    Int.

solveB() ->
    {ok, Data} = file:read_file("puzzles/puzzle03-input.txt"),
    Numbers = string:split(Data, <<"\n">>, all),
    solveB([ N || N <- Numbers, N =/= <<>>]).

solveB(Numbers) ->
    {binary_to_integer(solveB(Numbers, {most, 1}, 0), 2),
     binary_to_integer(solveB(Numbers, {least, 0}, 0), 2)}.

solveB([], _, _) ->
    [];
solveB([Answer], _, _) ->
    Answer;
solveB(Numbers, Preference, Offset) ->
    {Zeros, Ones} = lists:partition(
                      fun(<<_:Offset/binary, Bit, _/binary>>) ->
                              Bit == $0
                      end,
                      Numbers),
    Selection = case {length(Zeros) > length(Ones), Preference} of
                    {true, {most, _}} ->
                        Zeros;
                    {true, {least, _}} ->
                        Ones;
                    {false, {_, 1}} ->
                        Ones;
                    {false, {_, 0}} ->
                        Zeros
                end,
    solveB(Selection, Preference, Offset+1).
