%% Puzzle:
%%
%%

-module(puzzle11).

-export([
         solveA/0
        ]).

-compile([export_all]).

solveA() ->
    count_flashes(100, load_octopi()).

load_octopi() ->
    {ok, Data} = file:read_file("puzzles/puzzle11-input.txt"),
    [ [ C - $0 || C <- binary_to_list(Row) ]
      || Row <- string:split(Data, <<"\n">>, all), Row =/= <<>> ].

count_flashes(Iterations, Octopi) ->
    count_flashes(Iterations, Octopi, 0).

count_flashes(0, _, Count) ->
    Count;
count_flashes(Iterations, Octopi, Count) ->
    {Flashes, NewOctopi} = flash_round(increment_all(Octopi), 0),
    count_flashes(Iterations-1, NewOctopi, Count+Flashes).

increment_all(Octopi) ->
    [ [O+1 || O <- Row] || Row <- Octopi ].

clear_flashes(Octopi) ->
    [ [ case O of f -> 0; _ -> O end || O <- Row ] || Row <- Octopi ].

apply_mask([FirstOctopi|Octopi], [FirstMask,SecondMask|Mask], undef) ->
    [ [ case A of f -> f; _ -> A + B + C end
        || {A, B, C} <- lists:zip3(FirstOctopi, FirstMask, SecondMask) ]
    | apply_mask(Octopi, [SecondMask|Mask], FirstMask) ];
apply_mask([LastOctopi], [LastMask], PrevMask) ->
    [ [ case A of f -> f; _ -> A + B + C end
        || {A, B, C} <- lists:zip3(LastOctopi, LastMask, PrevMask) ] ];
apply_mask([Row|Octopi], [RowMask,NextMask|Mask], PrevMask) ->
    ZipMask = [ A + B + C
                || {A, B, C} <- lists:zip3(RowMask, NextMask, PrevMask) ],
    [ [ case A of f -> f; _ -> A + B end
        || {A, B} <- lists:zip(Row, ZipMask) ]
      | apply_mask(Octopi, [NextMask|Mask], RowMask) ].

flash_round(Octopi, Count) ->
    case flash_mask(Octopi) of
        {0, _, _} ->
            {Count, clear_flashes(Octopi)};
        {Flashes, Mask, NewOctopi} ->
            flash_round(apply_mask(NewOctopi, Mask, undef), Count+Flashes)
    end.

flash_mask(Octopi) ->
    {Counts, Mask, NewOctopi} =
        lists:unzip3([flash_mask_row(Row) || Row <- Octopi]),
    {lists:sum(Counts), Mask, NewOctopi}.

flash_mask_row(Row) ->
    flash_mask_row(Row, [], 0, []).

flash_mask_row([First|Octopi], [], 0, []) ->
    case First of
        f ->
            flash_mask_row(Octopi, [0,0], 0, [f]);
        N when N =< 9 ->
            flash_mask_row(Octopi, [0,0], 0, [N]);
        _ ->
            flash_mask_row(Octopi, [1,1], 1, [f])
    end;
flash_mask_row([Last], [This,Prev|Mask], Count, Rev) ->
    case Last of
        f ->
            {Count, lists:reverse([This,Prev|Mask]), lists:reverse([f|Rev])};
        N when N =< 9 ->
            {Count, lists:reverse([This,Prev|Mask]), lists:reverse([N|Rev])};
        _ ->
            {Count+1, lists:reverse([This+1,Prev+1|Mask]), lists:reverse([f|Rev])}
    end;
flash_mask_row([O|Octopi], [This,Prev|Mask], Count, Rev) ->
    case O of
        f ->
            flash_mask_row(Octopi, [0,This,Prev|Mask], Count, [f|Rev]);
        N when N =< 9 ->
            flash_mask_row(Octopi, [0,This,Prev|Mask], Count, [N|Rev]);
        _ ->
            flash_mask_row(Octopi, [1,This+1,Prev+1|Mask], Count+1, [f|Rev])
    end.
