%% Puzzle:
%%
%% cave network
%% https://adventofcode.com/2021/day/12
%%
%% explanation:
%% https://blog.beerriot.com/2021/12/12/advent-of-code-day-12/

-module(puzzle12).

-export([
         solveA/0,
         solveB/0,
         build_network/1,
         count_paths/2,
         count_paths/3
        ]).

solveA() ->
    count_paths(<<"start">>, load_network()).

solveB() ->
    count_paths(<<"start">>, load_network(), []).

load_network() ->
    {ok, Data} = file:read_file("puzzles/puzzle12-input.txt"),
    build_network([ string:split(Line, <<"-">>)
                    || Line <- string:split(Data, <<"\n">>, all),
                       Line =/= <<>> ]).

build_network(Connections) ->
    lists:foldl(fun([A,B], Net) ->
                        add_link(A, B, add_link(B, A, Net))
                end,
                [],
                Connections).

add_link(From, To, Net) ->
    case lists:keytake(From, 1, Net) of
        {value, {From, Links}, Rest} ->
            [{From, [To|Links]}|Rest];
        false ->
            [{From, [To]}|Net]
    end.

count_paths(<<"end">>, _) ->
  1;
count_paths(Cave, Network) ->
    case lists:keytake(Cave, 1, Network) of
        false ->
            0;
        {value, {Cave, Links}, Rest} ->
            RealRest = case Cave of
                           <<C, _/binary>> when C >= $A, C =< $Z ->
                               [{Cave, Links}|Rest];
                           _ ->
                               Rest
                       end,
            lists:sum([ count_paths(L, RealRest) || L <- Links ])
    end.

count_paths(<<"end">>, _, _) ->
  1;
count_paths(Cave, Network, Visited) ->
    case lists:keytake(Cave, 1, Network) of
        false ->
            0;
        {value, {Cave, Links}, Rest} ->
            {RealRest, NewVisited} =
                case Cave of
                    <<C, _/binary>> when C >= $A, C =< $Z ->
                        {[{Cave, Links}|Rest], Visited};
                    <<"start">> ->
                        {Rest, Visited};
                    _ ->
                        case Visited of
                            complete ->
                                {Rest, complete};
                            _ ->
                                case lists:member(Cave, Visited) of
                                    true ->
                                        {lists:foldl(
                                           fun(V, Acc) ->
                                                   lists:keydelete(V, 1, Acc)
                                           end,
                                           Rest,
                                           Visited),
                                         complete};
                                    false ->
                                        {[{Cave, Links}|Rest],
                                         [Cave|Visited]}
                                end
                        end
                end,
            lists:sum([ count_paths(L, RealRest, NewVisited)
                        || L <- Links ])
    end.
