%% Puzzle:
%%
%%

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
