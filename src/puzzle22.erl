%% Puzzle:
%%
%% parsing
%% https://adventofcode.com/2021/day/22

-module(puzzle22).

-export([
        ]).

-compile([export_all]).

load_file() ->
    {ok, Data} = file:read_file("puzzles/puzzle22-input.txt"),
    parse_input(Data).

parse_input(Data) ->
    [ parse_line(L) || L <- string:split(Data, <<"\n">>, all),
                       L =/= <<>> ].

parse_line(Line) ->
    [DirStr,CoordStr] = string:split(Line, <<" ">>),
    [<<"x=",XStr/binary>>,<<"y=",YStr/binary>>,<<"z=",ZStr/binary>>] =
        string:split(CoordStr, <<",">>, all),
    {case DirStr of <<"on">> -> on; <<"off">> -> off end,
     parse_range(XStr), parse_range(YStr), parse_range(ZStr)}.

parse_range(Str) ->
    [Start, End] = string:split(Str, <<"..">>),
    {binary_to_integer(Start), binary_to_integer(End)}.

limit_instructions(Inst, XRng, YRng, ZRng) ->
    lists:filter(fun({_Dir, XInst, YInst, ZInst}) ->
                         case {limit_range(XInst, XRng),
                               limit_range(YInst, YRng),
                               limit_range(ZInst, ZRng)} of
                             {{_,_},{_,_},{_,_}} ->
                                 true;
                             _ ->
                                 false
                         end
                 end,
                 Inst).

limit_range({XMin, XMax}, {RMin, RMax}) ->
    case (XMin >= RMin) and (XMax =< RMax) of
        true ->
            {XMin, XMax};
        false ->
            false
    end.

apply_instructions(Inst) ->
    apply_instructions(Inst, #{}).

apply_instructions([{Dir, {XMin,XMax}, {YMin,YMax}, {ZMin,ZMax}}|Rest],
                   Cubes) ->
    NewCubes = lists:foldl(
                 case Dir of
                     on ->
                         fun turn_on/2;
                     off ->
                         fun turn_off/2
                 end,
                 Cubes,
                 [{X,Y,Z}
                  || X <- lists:seq(XMin, XMax),
                     Y <- lists:seq(YMin, YMax),
                     Z <- lists:seq(ZMin, ZMax)]),
    apply_instructions(Rest, NewCubes);
apply_instructions([], Cubes) ->
    Cubes.

turn_on(Point, Cubes) ->
    Cubes#{Point => on}.

turn_off(Point, Cubes) ->
    maps:remove(Point, Cubes).

count_cubes_on(Cubes) ->
    maps:size(Cubes).
