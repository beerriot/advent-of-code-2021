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
                         lists:all(fun({I, R}) -> in_range(I, R) end,
                                   [{XInst,XRng},{YInst,YRng},{ZInst,ZRng}])
                 end,
                 Inst).

in_range({XMin, XMax}, {RMin, RMax}) ->
    (XMin >= RMin) and (XMax =< RMax).

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

apply_inst2([{on, X, Y, Z}|Rest], Cubes) ->
    apply_inst2(Rest, [{X, Y, Z}|Cubes]);
apply_inst2([{off, X, Y, Z}|Rest], Cubes) ->
    apply_inst2(Rest, subtract({X, Y, Z}, Cubes));
apply_inst2([], Cubes) ->
    unique(Cubes).

unique([C|Rest]) ->
    [C|unique(subtract(C, Rest))];
unique([]) ->
    [].

subtract(Cube, Cubes) ->
    lists:flatten([subtract2(Cube, C) || C <- Cubes]).

subtract2({{X1Min, X1Max}, {Y1Min, Y1Max}, {Z1Min, Z1Max}},
          {{X2Min, X2Max}, {Y2Min, Y2Max}, {Z2Min, Z2Max}}=C)
  when X1Max < X2Min; X1Min > X2Max;
       Y1Max < Y2Min; Y1Min > Y2Max;
       Z1Max < Z2Min; Z1Min > Z2Max ->
    %% not overlapping
    [C];
subtract2({{X1Min, X1Max}, {Y1Min, Y1Max}, {Z1Min, Z1Max}},
          {{X2Min, X2Max}, {Y2Min, Y2Max}, {Z2Min, Z2Max}})
  when X1Min =< X2Min, X1Max >= X2Max,
       Y1Min =< Y2Min, Y1Max >= Y2Max,
       Z1Min =< Z2Min, Z1Max >= Z2Max ->
    %% 100% overlap
    [];
subtract2({X1, Y1, Z1}=C, {X2, Y2, Z2}) ->
    Xs = split_axis(X1, X2),
    Ys = split_axis(Y1, Y2),
    Zs = split_axis(Z1, Z2),
    Splits = [ {X,Y,Z} || X <- Xs, Y <- Ys, Z <- Zs ],
    subtract(C, Splits).


%% 0   1   2   3
%% |-----------|
%%     |---|
%% |---|---|---|

split_axis({A1Min, A1Max}, {A2Min, A2Max}) ->
    As = min_compare(A1Min, A2Min) ++ max_compare(A1Max, A2Max),
    group_split(As).

group_split([AMin, AMax | Rest]) ->
    [{AMin, AMax}|group_split(Rest)];
group_split([]) ->
    [].

min_compare(P1, P2) when P1 =< P2 ->
    [P2];
min_compare(P1, P2) ->
    [P2, P1-1, P1].

max_compare(P1, P2) when P1 >= P2 ->
    [P2];
max_compare(P1, P2) ->
    [P1, P1+1, P2].

count_cubes_on2(Cubes) ->
    lists:foldl(fun({X, Y, Z}, Acc) ->
                        axis_size(X) * axis_size(Y) * axis_size(Z) + Acc
                end,
                0,
                Cubes).

axis_size({Min, Max}) ->
    (Max - Min) + 1.
