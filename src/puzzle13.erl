%% Puzzle:
%%
%% transparent paper folding
%% https://adventofcode.com/2021/day/13

-module(puzzle13).

-export([
         solveA/0,
         parse_input/1,
         fold_paper/2
        ]).

solveA() ->
    {Dots, [FirstFold|_]} = load_file(),
    fold_paper(Dots, [FirstFold]).

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
