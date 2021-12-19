%% Puzzle:
%%
%% Beacons
%% https://adventofcode.com/2021/day/19

-module(puzzle19).

-export([
         load_file/0,
         parse_scanners/1,
         find_overlap/2,
         align_stations/1
        ]).

-compile([export_all]).

-record(s,     %% scanner
        {n,    %% number
         a,    %% alignment (debugging)
         bs}). %% beacons

-record(b,     %% beacon
        {x,
         y,
         z}).

solveA() ->
    count_unique_beacons(align_stations(load_file())).

load_file() ->
    {ok, Data} = file:read_file("puzzles/puzzle19-input.txt"),
    parse_scanners(Data).

parse_scanners(Input) ->
    parse_next_scanner(string:split(Input, <<"\n">>, all), []).

parse_next_scanner([<<"--- scanner", _/binary>>|Rest], Scanners) ->
    {Beacons, RestB} = parse_beacons(Rest, []),
    parse_next_scanner(RestB, [#s{n=length(Scanners), bs=Beacons}|Scanners]);
parse_next_scanner([], Scanners) ->
    Scanners.

parse_beacons([<<>>|Rest], Beacons) ->
    {Beacons, Rest};
parse_beacons([Text|Rest], Beacons) ->
    [X,Y,Z] = [binary_to_integer(N)
               || N <- string:split(Text, <<",">>, all)],
    parse_beacons(Rest, [#b{x=X,y=Y,z=Z}|Beacons]).

%% what axis of S2 goes along the X axis of S1?
all_rotations() ->
    [{#b.x, 1}, {#b.x, -1},
     {#b.y, 1}, {#b.y, -1},
     {#b.z, 1}, {#b.z, -1}].

%% what axis of S2 goes along the Y axis of S1, given the S2 axis
%% along S1's X
rotations_for_selection({Axis, _}) ->
    %% pinning one axis still allows selection of either in either direction
    [ R || R={RA,_} <- all_rotations(), RA =/= Axis ].

%% what axis of S2 goes along the Z axis of S1, given the S2 axes
%% along S1's X and Y
%% right-hand rule, long form
rotations_for_selection({#b.x, X}, {#b.y, Y}) ->
    %% basically the XOR
    [{#b.z, X*Y}];
rotations_for_selection({#b.y, Y}, {#b.x, X}) ->
    %% NOT XOR
    [{#b.z, -1*Y*X}];
rotations_for_selection({#b.x, X}, {#b.z, Z}) ->
    [{#b.y, -1*X*Z}];
rotations_for_selection({#b.z, Z}, {#b.x, X}) ->
    [{#b.y, Z*X}];
rotations_for_selection({#b.y, Y}, {#b.z, Z}) ->
    [{#b.x, Y*Z}];
rotations_for_selection({#b.z, Z}, {#b.y, Y}) ->
    [{#b.x, -1*Y*Z}].

find_overlap(S1, S2) ->
    case find_overlap(S1#s.bs, S2#s.bs, #b.x, all_rotations()) of
        [] ->
            [];
        XOverlaps ->
            find_overlap_x(S1, S2, XOverlaps)
    end.

find_overlap_x(S1, S2, [{XSel, XOffset}|Rest]) ->
    case find_overlap(S1#s.bs, S2#s.bs, #b.y, rotations_for_selection(XSel)) of
        [] ->
            find_overlap_x(S1, S2, Rest);
        YOverlaps ->
            find_overlap_xy(S1, S2, {XSel, XOffset}, YOverlaps)
                ++ find_overlap_x(S1, S2, Rest)
    end;
find_overlap_x(_, _, []) ->
    [].

find_overlap_xy(S1, S2, XSO, [{YSel, YOffset}|Rest]) ->
    case find_overlap(S1#s.bs, S2#s.bs, #b.z,
                      rotations_for_selection(element(1, XSO), YSel)) of
        [] ->
            find_overlap_xy(S1, S2, XSO, Rest);
        ZOverlaps ->
            find_overlap_xyz(S1, S2, XSO, {YSel, YOffset}, ZOverlaps)
                ++ find_overlap_xy(S1, S2, XSO, Rest)
    end;
find_overlap_xy(_, _, _, []) ->
    [].

find_overlap_xyz(S1, S2, XSO, YSO, [{ZSel, ZOffset}|Rest]) ->
    ZS2 = move_points(#b{x=XSO, y=YSO, z={ZSel, ZOffset}}, S2#s.bs),
    case length(find_matching(S1#s.bs, ZS2)) of
        L when L >= 12 ->
            io:format("Confirmed overlap of L=~p~n", [L]),
            [#b{x=XSO, y=YSO, z={ZSel, ZOffset}}]
                ++ find_overlap_xyz(S1, S2, XSO, YSO, Rest);
        L ->
            io:format("Rejecting at final stage becase L=~p~n", [L]),
            find_overlap_xyz(S1, S2, XSO, YSO, Rest)
    end;
find_overlap_xyz(_, _, _, _, []) ->
    [].

find_overlap(BS1, BS2, FS1, [Sel|Rest]) ->
    case maps:keys(
           maps:filter(fun(_, V) -> V >= 12 end,
                       offset_histo(FS1, BS1, Sel, BS2))) of
        [] ->
            find_overlap(BS1, BS2, FS1, Rest);
        Found ->
            %% it's easier to find all overlaps on this axis now than
            %% to come back to it if the first doesn't work
            [{Sel, F} || F <- Found] ++ find_overlap(BS1, BS2, FS1, Rest)
    end;
find_overlap(_, _, _, []) ->
    [].

offset_histo(FBS1, BS1, {FBS2, M}, BS2) ->
    lists:foldl(
      fun(V, Acc) ->
              Offsets = lists:usort([ element(FBS1, B) - V || B <- BS1 ]),
              maps:merge_with(fun(_, C1, C2) -> C1 + C2 end,
                              maps:from_list([{O, 1} || O <- Offsets]),
                              Acc)
      end,
      #{},
      [ M * element(FBS2, B) || B <- BS2 ]).

%% This is set-intersection, but getting into and out of Map format to
%% use intersect_with seemed more complicated
find_matching(BS1, BS2) ->
    ordsets:intersection(ordsets:from_list(BS1), ordsets:from_list(BS2)).

move_points(#b{x={{XSel,XMul},XOff},
               y={{YSel,YMul},YOff},
               z={{ZSel,ZMul},ZOff}},
            Beacons) ->
    [#b{x=XMul * element(XSel, B) + XOff,
        y=YMul * element(YSel, B) + YOff,
        z=ZMul * element(ZSel, B) + ZOff}
     || B <- Beacons].

align_stations(Stations) ->
    {value, S0, Rest} = lists:keytake(0, #s.n, Stations),
    align_stations(Rest,
                   [S0#s{a=#b{x={{#b.x,1},0},
                              y={{#b.y,1},0},
                              z={{#b.z,1},0}}}],
                   []).

align_stations([S|Rest], Aligned, Retry) ->
    case find_alignment(S, Aligned) of
        none ->
            io:format("Warning: moving station ~p to back~n", [S#s.n]),
            align_stations(Rest, Aligned, [{length(Aligned), S}|Retry]);
        A ->
            Moved = move_points(A, S#s.bs),
            align_stations(Rest, [S#s{a=A, bs=Moved}|Aligned], Retry)
    end;
align_stations([], Aligned, []) ->
    Aligned;
align_stations([], Aligned, Retry) ->
    case lists:any(fun({L,_}) -> L < length(Aligned) end, Retry) of
        true ->
            io:format("Retrying stations ~p~n", [[S#s.n || {_, S} <- Retry]]),
            align_stations([S || {_, S} <- Retry], Aligned, []);
        false ->
            {Aligned, Retry}
    end.

find_alignment(Su, [Sa|Rest]) ->
    case find_overlap(Sa, Su) of
        [] ->
            find_alignment(Su, Rest);
        [A] ->
            %% this will fail to match if puzzle is designed such that
            %% two stations can line up in multiple ways
            io:format("Station ~p aligned with ~p~n", [Su#s.n, Sa#s.n]),
            A
    end;
find_alignment(_, []) ->
    none.

count_unique_beacons(Stations) ->
    length(ordsets:union([ordsets:from_list(S#s.bs) || S <- Stations])).
