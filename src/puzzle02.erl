%% Puzzle:
%%
%% First half: Add "forward" directives, and net up-down directives.
%%
%% Second half: up/down is aim, not depth, and depth changes as forward*aim
%%
%% https://adventofcode.com/2021/day/2
%%
%% explanation:
%% https://blog.beerriot.com/2021/12/13/advent-of-code-day-2/

-module(puzzle02).

-export([
         solveA/0,
         solveA/1,
         solveB/0,
         solveB/1
        ]).

-record(stateA, {x, depth}).

solveA() ->
    {ok, Data} = file:read_file("puzzles/puzzle02-input.txt"),
    solveA(string:split(Data, <<"\n">>, all)).

solveA(Commands) ->
    reportA(Commands, #stateA{x=0, depth=0}).

reportA([<<>>], Position) ->
    %% file ends with a newline
    Position;
reportA([], Position) ->
    %% my copy-paste of the example doe not end with a newline
    Position;
reportA([<<"forward ", Move/binary>>|Rest], State=#stateA{x=X}) ->
    reportA(Rest, State#stateA{x=X + binary_to_integer(Move)});
reportA([<<"down ", Move/binary>>|Rest], State=#stateA{depth=Depth}) ->
    reportA(Rest, State#stateA{depth=Depth + binary_to_integer(Move)});
reportA([<<"up ", Move/binary>>|Rest], State=#stateA{depth=Depth}) ->
    reportA(Rest, State#stateA{depth=Depth - binary_to_integer(Move)}).


-record(stateB, {x, depth, aim}).

solveB() ->
    {ok, Data} = file:read_file("puzzles/puzzle02-input.txt"),
    solveB(string:split(Data, <<"\n">>, all)).

solveB(Commands) ->
    lists:foldl(fun(<<"forward ", Move/binary>>,
                    State=#stateB{x=X, depth=Depth, aim=Aim}) ->
                        M = binary_to_integer(Move),
                        State#stateB{x=X + M, depth=Depth + Aim * M};
                   (<<"down ", Move/binary>>, State=#stateB{aim=Aim}) ->
                        State#stateB{aim=Aim + binary_to_integer(Move)};
                   (<<"up ", Move/binary>>, State=#stateB{aim=Aim}) ->
                        State#stateB{aim=Aim - binary_to_integer(Move)};
                   (<<>>, State) ->
                        %% file ends with a newline
                        State
                end,
                #stateB{x=0, depth=0, aim=0},
                Commands).
