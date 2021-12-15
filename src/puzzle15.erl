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

-compile([export_all]).

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

cost_to_point({{Width,Height},_}) ->
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
    find_lowest_risk(Territory,
                     [ {[{0,0}], 0, distance_to_goal({0,0}, Territory)} ],
                     cost_to_point(Territory), 0).

find_lowest_risk(A, [Cheapest|_]=B, C, 1000) ->
    io:format("Current Cheapest: ~p~n", [Cheapest]),
    find_lowest_risk(A, B, C, 0);
find_lowest_risk({{Width,Height},_}=Territory, [Cheapest|Rest],CostMatrix,C) ->
    {[{LX, LY}|_], Risk, _} = Cheapest,
    case length(Rest) > 100000 of true -> {stop, Rest}; _ ->
    case {Width-LX, Height-LY} of
        {1,2} ->
            io:format("Paths left to search: ~p~n", [length(Rest)]),
            %% just north of end
            {Risk + risk_at({Width-1, Height-1}, Territory), Cheapest, Rest};
        {2,1} ->
            io:format("Paths left to search: ~p~n", [length(Rest)]),
            %% just west of end
            {Risk + risk_at({Width-1, Height-1}, Territory), Cheapest, Rest};
        _ ->
            {NewPaths, NewCostMatrix} = advance_path(Cheapest, Territory, CostMatrix),
            find_lowest_risk(Territory,
                             add_paths(NewPaths, Rest), NewCostMatrix, C+1)
    end end.

advance_path({[{LX,LY}|_]=Path, Risk, _},
             {{Width, Height},_}=Territory,
             CostMatrix) ->
    NewPoints = [ {X,Y}
                  || {X,Y} <- [{LX+1,LY},{LX-1,LY},
                               {LX,LY+1},{LX,LY-1}],
                     X >= 0, X < Width,
                     Y >= 0, Y < Height ],
    NoReturns = lists:filter(fun(Step) -> not lists:member(Step, Path) end,
                             NewPoints),
    NewPaths = [ {[Step|Path],
                  Risk + risk_at(Step, Territory),
                  distance_to_goal(Step, Territory)}
                 || Step <- NoReturns ],
    {Shorter, NewMatrix} =
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
    {lists:sort(fun({_,VA,DA},{_,VB,DB}) ->
                        (VA+DA) =< (VB+DB)
                end,
                Shorter),
     NewMatrix}.

add_paths(A, B) ->
    lists:merge(fun({_,VA,DA}, {_,VB,DB}) ->
                        (VA+DA) =< (VB+DB)
                end, A, B).
%%     add_paths(A, B, []).

%% add_paths([{[PA|_],VA,DA}=A|RA]=LA,
%%           [{[PB|_],VB,DB}=B|RB]=LB, Acc) ->
%%     case (VA+DA) < (VB+DB) of
%%         true ->
%%             case lists:any(fun({[PX|_], VX, _}) ->
%%                                    (PX == PA) and (VX =< VA)
%%                            end, Acc) of
%%                 true ->
%%                     %% already have a lower-risk path to here
%%                     add_paths(RA, LB, Acc);
%%                 false ->
%%                     add_paths(RA, LB, [A|Acc])
%%                 end;
%%         false ->
%%             case lists:any(fun({[PX|_], VX, _}) ->
%%                                    (PX == PB) and (VX =< VB)
%%                            end, Acc) of
%%                 true ->
%%                     %% already have a lower-risk path to here
%%                     add_paths(LA, RB, Acc);
%%                 false ->
%%                     add_paths(LA, RB, [B|Acc])
%%             end
%%     end;
%% add_paths([], LB, Acc) ->
%%     lists:reverse(Acc) ++ LB;
%% add_paths(LA, [], Acc) ->
%%     lists:reverse(Acc) ++ LA.

risk_at({X, Y}, {{Width, _}, Territory}) ->
    <<_:(Y*Width + X)/binary, Risk, _/binary>> = Territory,
    Risk.

distance_to_goal({X,Y}, {{Width, Height},_}=Territory) ->
    (((Width-X)+(Height-Y))-2).% * 2.

%%         ((sum_risk_from({X,Y}, Territory) div ((Width-X)*(Height-Y))) - 1).

%% sum_risk_from({X, Y}, {{Width, Height}, Risk}=Territory) when Y < Height ->
%%     <<_:(Y*Width + X)/binary, Line:(Width-X)/binary, _/binary>> = Risk,
%%     lists:sum([ R || <<R>> <= Line ])
%%         + sum_risk_from({X, Y+1}, Territory);
%% sum_risk_from(_, _) -> 0.
