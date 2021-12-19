%% Puzzle:
%%
%% Seven-segment displays
%% https://adventofcode.com/2021/day/8
%%
%% explanation:
%% https://blog.beerriot.com/2021/12/18/advent-of-code-day-8/

-module(puzzle08).

-export([
         solveA/0,
         solveB/0,
         parse_display/1,
         count_1478_outputs/1,
         read_display/1,
         map_segments/1,
         decode_output/2
        ]).

-compile([export_all]).

solveA() ->
    count_1478_outputs(load_displays()).

load_displays() ->
    {ok, Data} = file:read_file("puzzles/puzzle08-input.txt"),
    [ parse_display(D) || D <- string:split(Data, <<"\n">>, all),
                          D =/= <<>> ].

parse_display(D) ->
    {Patterns, [<<"|">>|Output]} =
        lists:splitwith(fun(C) -> C =/= <<"|">> end,
                        string:split(D, <<" ">>, all)),
    {Patterns, Output}.

count_1478_outputs(Displays) ->
    lists:sum([ length([ O || O <- Output, is_1478(O) ])
                || {_, Output} <- Displays ]).

is_1478(O) ->
    case size(O) of
        2 -> true; % 1
        4 -> true; % 4
        3 -> true; % 7
        7 -> true; % 8
        _ -> false
    end.

solveB() ->
    Displays = load_displays(),
    lists:foldl(fun(D, Acc) ->
                        Acc + read_display(D)
                end,
                0,
                Displays).

read_display({RawInputs, RawOutputs}) ->
    Inputs = [lists:sort(binary_to_list(I)) || I <- RawInputs],
    {Segments, _} = map_segments(Inputs),
    Outputs = [lists:sort(binary_to_list(O)) || O <- RawOutputs],
    decode_output(Outputs, Segments).

map_segments(Inputs) ->
    One = find_one(Inputs),
    Seven = find_seven(Inputs),
    [A] = Seven -- One,
    Six = find_six(Inputs, One),
    [C] = One -- Six,
    [F] = One -- [C],
    Four = find_four(Inputs),
    Nine = find_nine(Inputs, Four),
    Zero = find_zero(Inputs, Six, Nine),
    [D] = Four -- Zero,
    [B] = Four -- [C, F, D],
    Eight = find_eight(Inputs),
    [E] = Eight -- Nine,
    [G] = Eight -- [A, B, C, D, E, F],
    Two = find_two(Inputs, B, F),
    Three = find_three(Inputs, B, E),
    Five = find_five(Inputs, C, E),
    {[Zero, One, Two, Three, Four, Five, Six, Seven, Eight, Nine],
     [A, B, C, D, E, F, G]}.

find_one(Inputs) -> hd([_]=find_inputs_of_length(Inputs, 2)).
find_seven(Inputs) -> hd([_]=find_inputs_of_length(Inputs, 3)).
find_four(Inputs) -> hd([_]=find_inputs_of_length(Inputs, 4)).
find_eight(Inputs) -> hd([_]=find_inputs_of_length(Inputs, 7)).
find_inputs_of_length(Inputs, Length) ->
    [ Code || Code <- Inputs, length(Code) == Length ].

find_six(Inputs, One) ->
    hd([_]=[ Code || Code <- Inputs,
                     length(Code) == 6,
                     length(One -- Code) == 1]).

find_nine(Inputs, Four) ->
    hd([_]=[ Code || Code <- Inputs,
                     length(Code) == 6,
                     length(Four -- Code) == 0 ]).

find_zero(Inputs, Six, Nine) ->
    hd([_]=[ Code || Code <- Inputs,
                     length(Code) == 6,
                     Code =/= Six,
                     Code =/= Nine ]).

find_two(Inputs, B, F) ->
    hd([_]=[ Code || Code <- Inputs,
                     length(Code) == 5,
                     length(Code -- [B,F]) == 5 ]).

find_three(Inputs, B, E) ->
    hd([_]=[ Code || Code <- Inputs,
                     length(Code) == 5,
                     length(Code -- [B,E]) == 5 ]).

find_five(Inputs, C, E) ->
    hd([_]=[ Code || Code <- Inputs,
                     length(Code) == 5,
                     length(Code -- [C,E]) == 5 ]).

decode_output(Output, Segments) ->
    lists:foldl(fun(D, Acc) ->
                        N = decode_digit(D, Segments),
                        N + Acc * 10
                end,
                0,
                Output).

decode_digit(Zero, [Zero |_]) -> 0;
decode_digit(One, [_, One |_]) ->  1;
decode_digit(Two, [_, _, Two |_]) -> 2;
decode_digit(Three, [_, _, _, Three |_]) -> 3;
decode_digit(Four, [_, _, _, _, Four |_]) -> 4;
decode_digit(Five, [_, _, _, _, _, Five |_]) -> 5;
decode_digit(Six, [_, _, _, _, _, _, Six |_]) -> 6;
decode_digit(Seven, [_, _, _, _, _, _, _, Seven |_]) -> 7;
decode_digit(Eight, [_, _, _, _, _, _, _, _, Eight |_]) -> 8;
decode_digit(Nine, [_, _, _, _, _, _, _, _, _, Nine]) -> 9.


%% acedgfb  8
%% cdfbe      235
%% gcdfa      235
%% fbcad      235
%% dab      7
%% cefabd     069
%% cdfgeb     069
%% eafb     4
%% cagedb     069
%% ab       1
%% |
%% cdfeb
%% fcadb
%% cdfeb
%% cdbaf

%% 1 & 4 & 7 share cf -> ab=cf, d=a
%% abcdef
%% abcde g are 09 (shared cf) -> gf=de
%%  bcdefg is 6 -> a = c
%%  bcdef
%% a cd fg are 25 -> agce=bfce
%% abcd f  is 5

%% code length 2 is 1 -> chars are   C F
%% code length 3 is 7 -> char  not   C F is  A
%% code length 4 is 4 -> chars not   C F are  B D
%% code length 7 is 8 -> chars not ABCDF are     E G
%% code length 5 without one of CF and one of EG is 5 -> confirm CF and EG
%% code length 5 without one of BD and one of CF is 2 -> confirm CF and BD
%% code length 5 without one of BD and one of EG is 3 -> confirm BD and EG
%% code length 6 without one of BD is 0 - confirm D
%% code length 6 without one of EG is 9 - confirm E
%% code length 6 without one of CF is 6 - confirm C
