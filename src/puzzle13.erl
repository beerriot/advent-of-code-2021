%% Puzzle:
%%
%% transparent paper folding
%% https://adventofcode.com/2021/day/13
%%
%% explanation:
%% https://blog.beerriot.com/2021/12/13/advent-of-code-day-13/

-module(puzzle13).

-export([
         solveA/0,
         solveB/0,
         parse_input/1,
         fold_paper/2,
         render_dots/1
        ]).

solveA() ->
    {Dots, [FirstFold|_]} = load_file(),
    fold_paper(Dots, [FirstFold]).

solveB() ->
    {Dots, Folds} = load_file(),
    FinalDots = fold_paper(Dots, Folds),
    render_dots(FinalDots).

load_file() ->
    {ok, Data} = file:read_file("puzzles/puzzle13-input.txt"),
    parse_input(Data).

parse_input(Data) ->
    {DotLines, [<<>>|FoldLines]} =
        lists:splitwith(fun(L) -> L =/= <<>> end,
                        string:split(Data, <<"\n">>, all)),
    {parse_dots(DotLines), parse_folds(FoldLines)}.

parse_dots(DotLines) ->
    ordsets:from_list([ begin
                            [X,Y] = string:split(L, <<",">>),
                            {binary_to_integer(X), binary_to_integer(Y)}
                        end
                        || L <- DotLines ]).

parse_folds(FoldLines) ->
    [ begin
          [XY, V] = string:split(Axis, <<"=">>),
          {case XY of <<"x">> -> 1; <<"y">> -> 2 end,
           binary_to_integer(V)}
      end
      || <<"fold along ", Axis/binary>> <- FoldLines ].

fold_paper(Dots, []) ->
    Dots;
fold_paper(Dots, [{Axis,Value}|Rest]) ->
    {AboveLeft, BelowRight} =
        lists:partition(fun(D) -> element(Axis, D) < Value end, Dots),
    Moved = [ setelement(Axis, D,
                         element(Axis, D) - 2 * (element(Axis, D) - Value))
              || D <- BelowRight ],
    fold_paper(ordsets:union(AboveLeft, ordsets:from_list(Moved)), Rest).

render_dots(Dots) ->
    Width = 1 + lists:max([ X || {X,_} <- Dots ]),
    Height = 1 + lists:max([ Y || {_, Y} <- Dots ]),
    Image = lists:duplicate(Height, lists:duplicate(Width, $.)),
    render_dots(Dots, Image).

render_dots([], Image) ->
    lists:foreach(fun(L) -> io:format("~s~n", [L]) end, Image),
    Image;
render_dots([{X,Y}|Rest], Image) ->
    {RowsBefore, [Row|RowsAfter]} = lists:split(Y, Image),
    {ColumnsBefore, [_|ColumnsAfter]} = lists:split(X, Row),
    render_dots(Rest,
                RowsBefore ++ [(ColumnsBefore ++ [$#|ColumnsAfter])
                               |RowsAfter]).
