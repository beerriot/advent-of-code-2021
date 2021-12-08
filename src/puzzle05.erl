%% Puzzle:
%%
%% Lines

-module(puzzle05).

-export([
         solveA/0,
         solveB/0,
         map_lines/1,
         count_overlaps/1
        ]).

solveA() ->
    {ok, Data} = file:read_file("puzzles/puzzle05-input.txt"),
    Lines = [ parse_line(S) || S <- string:split(Data, <<"\n">>, all),
                               S =/= <<>> ],
    HVLines = [ L || L={{X1,Y1},{X2,Y2}} <- Lines,
                     (X1 == X2) orelse (Y1 == Y2) ],
    Map = map_lines(HVLines),
    count_overlaps(Map).

solveB() ->
    {ok, Data} = file:read_file("puzzles/puzzle05-input.txt"),
    Lines = [ parse_line(S) || S <- string:split(Data, <<"\n">>, all),
                               S =/= <<>> ],
    Map = map_lines(Lines),
    count_overlaps(Map).

parse_line(String) ->
    [Start, End] = string:split(String, <<" -> ">>),
    {parse_point(Start), parse_point(End)}.

parse_point(String) ->
    [X, Y] = string:split(String, <<",">>),
    {binary_to_integer(X), binary_to_integer(Y)}.

map_lines(Lines) ->
    {MaxX, MaxY} = lists:foldl(fun({{X1,Y1},{X2,Y2}}, {MX, MY}) ->
                                       {lists:max([X1, X2, MX]),
                                        lists:max([Y1, Y2, MY])}
                               end,
                               {0, 0},
                               Lines),
    Map = lists:duplicate(MaxY+1, lists:duplicate(MaxX+1, 0)),
    lists:foldl(fun map_line/2, Map, Lines).

map_line({{X1, Y}, {X2, Y}}, Map) ->
    %% horizontal
    {Above, [Row|Below]} = lists:split(Y, Map),
    [MinX, MaxX] = lists:sort([X1, X2]),
    {NewRow, _} = lists:mapfoldl(
                    fun(V, X) ->
                            {case (X >= MinX andalso X =< MaxX) of
                                 true -> V+1;
                                 false -> V
                             end,
                             X+1}
                    end, 0, Row),
    Above ++ [NewRow|Below];
map_line({{X, Y1}, {X, Y2}}, Map) ->
    %% vertical
    [MinY, MaxY] = lists:sort([Y1, Y2]),
    element(1,
            lists:mapfoldl(
              fun(Row, Y) ->
                      case (Y >= MinY andalso Y =< MaxY) of
                          true ->
                              {Left, [V|Right]} = lists:split(X, Row),
                              NewRow = Left ++ [V+1|Right];
                          false ->
                              NewRow = Row
                      end,
                      {NewRow, Y+1}
              end,
              0,
              Map));
map_line({{X1, Y1}, {X2, Y2}}, Map) ->
    %% diagonal
    [{StartX, StartY}, {EndX, EndY}] = lists:keysort(2, [{X1, Y1}, {X2, Y2}]),
    XDir = case EndX > StartX of
               true -> 1;
               false -> -1
           end,
    element(1,
            lists:mapfoldl(
              fun(Row, Y) ->
                      case (Y >= StartY andalso Y =< EndY) of
                          true ->
                              X = StartX + XDir * (Y - StartY),
                              {Left, [V|Right]} = lists:split(X, Row),
                              NewRow = Left ++ [V+1|Right];
                          false ->
                              NewRow = Row
                      end,
                      {NewRow, Y+1}
              end,
              0,
              Map)).


count_overlaps(Map) ->
    lists:sum([ length([ N || N <- Row, N > 1 ]) || Row <- Map ]).
