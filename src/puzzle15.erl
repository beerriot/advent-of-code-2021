%% Puzzle:
%%
%% transparent paper folding
%% https://adventofcode.com/2021/day/15
%%
%% explanation:
%% https://blog.beerriot.com/2021/12/15/advent-of-code-day-15/

-module(puzzle15).

-export([
         solveA/0,
         parse_input/1,
         find_lowest_risk/1
        ]).

solveA() ->
    Territory = load_file(),
    find_lowest_risk(Territory).

load_file() ->
    {ok, Data} = file:read_file("puzzles/puzzle15-input.txt"),
    parse_input(Data).

parse_input(Data) ->
    Width = width(Data),
    Risks = << <<(N - $0)>> || <<N>> <= Data, N >= $0, N =< $9 >>,
    Height = size(Risks) div Width,
    {{Width, Height}, Risks}.

width(<<$\n, _/binary>>) ->
    0;
width(<<_, Rest/binary>>) ->
    1 + width(Rest).

init_cost_to_point({{Width,Height},_}) ->
    [ [ 9*(X+Y) || X <- lists:seq(0, Width-1) ]
      || Y <- lists:seq(0, Height-1) ].

review_cost_to_point({X,Y}, NewCost, CostMatrix) ->
    {Above, [Row|Below]} = lists:split(Y, CostMatrix),
    {Left, [Elt|Right]} = lists:split(X, Row),
    case NewCost < Elt of
        true ->
            {change, Above ++ [ (Left ++ [NewCost|Right]) | Below ]};
        false ->
            nochange
    end.

find_lowest_risk(Territory) ->
    InitialPath = {[{0,0}], 0, distance_to_goal({0,0}, Territory)},
    find_lowest_risk(Territory,
                     [ InitialPath ],
                     init_cost_to_point(Territory)).

find_lowest_risk({{Width,Height},_}=Territory,
                 [Cheapest|Rest],
                 CostMatrix) ->
    {[{LX, LY}|_], Risk, _} = Cheapest,
    case {Width-LX, Height-LY} of
        {1,2} ->
            %% just north of end
            {Risk + risk_at({Width-1, Height-1}, Territory), Cheapest};
        {2,1} ->
            %% just west of end
            {Risk + risk_at({Width-1, Height-1}, Territory), Cheapest};
        _ ->
            {NewPaths, NewCostMatrix} =
                advance_path(Cheapest, Territory, CostMatrix),
            find_lowest_risk(Territory,
                             add_paths(NewPaths, Rest),
                             NewCostMatrix)
    end.

advance_path({[{LX,LY}|_]=Path, Risk, _},
             {{Width, Height},_}=Territory,
             CostMatrix) ->
    NewPoints = [ {X,Y}
                  || {X,Y} <- [{LX+1,LY},{LX-1,LY},
                               {LX,LY+1},{LX,LY-1}],
                     X >= 0, X < Width,
                     Y >= 0, Y < Height ],
    NewPaths = [ {[Step|Path],
                  Risk + risk_at(Step, Territory),
                  distance_to_goal(Step, Territory)}
                 || Step <- NewPoints ],
    {Cheaper, NewMatrix} =
        lists:foldl(fun({[Step|_], R, _}=P, {AccP, AccM}) ->
                            case review_cost_to_point(Step, R, AccM) of
                                {change, NewAccM} ->
                                    {[P|AccP], NewAccM};
                                nochange ->
                                    {AccP, AccM}
                            end
                    end,
                    {[], CostMatrix},
                    NewPaths),
    {lists:sort(fun order_paths/2, Cheaper),
     NewMatrix}.

add_paths(A, B) ->
    lists:merge(fun order_paths/2, A, B).

order_paths({_,VA,DA}, {_,VB,DB}) ->
    (VA+DA) =< (VB+DB).

risk_at({X, Y}, {{Width, _}, Territory}) ->
    <<_:(Y*Width + X)/binary, Risk, _/binary>> = Territory,
    Risk.

distance_to_goal({X,Y}, {{Width, Height},_}) ->
    (((Width-X)+(Height-Y))-2).
