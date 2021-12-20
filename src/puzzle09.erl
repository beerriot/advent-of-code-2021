%% Puzzle:
%%
%% Basins
%% https://adventofcode.com/2021/day/9
%%
%% explanation:
%% https://blog.beerriot.com/2021/12/20/advent-of-code-day-9/

-module(puzzle09).

-export([
         solveA/0,
         buffer_map/1,
         score_low_spots/1,
         score_row/1
        ]).

-compile([export_all]).

solveA() ->
    score_low_spots(buffer_map(load_map())).

load_map() ->
    {ok, Data} = file:read_file("puzzles/puzzle09-input.txt"),
    [ [ C - $0 || C <- binary_to_list(D) ]
      || D <- string:split(Data, <<"\n">>, all),
         D =/= <<>> ].

buffer_map([Head|_]=Map) ->
    DummyRow = lists:duplicate(length(Head)+2, 9),
    [DummyRow] ++
        [ [9] ++ R ++ [9] || R <- Map ]
        ++ [DummyRow].

score_low_spots([Above,This,Below|Rest]) ->
    ZipRow = lists:zip3(Above, This, Below),
    score_row(ZipRow) + score_low_spots([This,Below|Rest]);
score_low_spots(_) ->
    0.

score_row([{_, Left, _},{Above, This, Below}=T,{_, Right, _}=N|Rest])
  when This < Left,
       This < Above,
       This < Below,
       This < Right ->
    This + 1 + score_row([T,N|Rest]);
score_row([_|Rest]) ->
    score_row(Rest);
score_row([]) ->
    0.

solveB() ->
    Map = load_map(),
    Basins = find_basins(Map),
    [{_, _, A},{_, _, B},{_, _, C}|_] =
        lists:reverse(lists:keysort(3, Basins)),
    A * B * C.

find_basins(Map) ->
    RowBasins = find_row_basins(Map, []),
    group_row_basins(RowBasins, []).


find_row_basins([Row|Rest], Acc) ->
    case lists:foldl(fun(9, {undef, L, Basins}) ->
                             {undef, L+1, Basins};
                        (9, {Start, L, Basins}) ->
                             {undef, L+1, [{Start, L-1}|Basins]};
                        (_, {undef, L, Basins}) ->
                             {L, L+1, Basins};
                        (_, {Start, L, Basins}) ->
                             {Start, L+1, Basins}
                     end,
                     {undef, 0, []},
                     Row) of
        {undef, _, Basins} ->
            find_row_basins(Rest, [Basins | Acc]);
        {Start, L, Basins} ->
            find_row_basins(Rest, [ [{Start, L-1}|Basins] | Acc])
    end;
find_row_basins([], Acc) ->
    Acc.

group_row_basins([Row|Rest], Acc) ->
    group_row_basins(Rest, group_row(Row, Acc));
group_row_basins([], Acc) ->
    Acc.

group_row([{Start,End}|Rest], Acc) ->
    {Overlapping, NotOverlapping} =
        lists:partition(
          fun({Spans, _, _}) ->
                  lists:any(
                    fun({SpanStart, SpanEnd}) ->
                            ((SpanStart =< Start) and (Start =< SpanEnd)) or
                                ((SpanStart =< End) and (End =< SpanEnd)) or
                                ((Start =< SpanStart) and (SpanStart =< End)) or
                                ((Start =< SpanEnd) and (SpanEnd =< End))
                    end,
                    Spans)
          end,
          Acc),
    case Overlapping of
        [] ->
            NewOverlapping = {[], [{Start, End}], End-Start+1};
        _ ->
            NewOverlapping =
                lists:foldl(
                  fun({Spans, NextSpans, Count},
                      {AccSpans, AccNextSpans, AccCount}) ->
                          {Spans ++ AccSpans,
                           NextSpans ++ AccNextSpans,
                           Count+AccCount}
                  end,
                  {[], [], 0},
                  [{[],[{Start,End}], End-Start+1}|Overlapping])
    end,
    group_row(Rest, [NewOverlapping|NotOverlapping]);
group_row([], Acc) ->
    [{NextSpans, [], Count} || {_, NextSpans, Count} <- Acc].
