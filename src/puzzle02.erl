%% Puzzle:
%%
%% First half: Add "forward" directives, and net up-down directives.
%%
%% Second half: up/down is aim, not depth, and depth changes as forward*aim

-module(puzzle02).

-export([
         solveA/0,
         solveA/1,
         solveB/0,
         solveB/1
        ]).

-record(stateA, {x, depth}).

solveA() ->
    solveA("puzzles/puzzle02-input.txt").

solveA(Filename) ->
    {ok, Data} = file:read_file(Filename),
    Commands = string:split(Data, <<"\n">>, all),
    reportA(Commands, #stateA{x=0, depth=0}).

reportA([<<>>], Position) ->
    %% file ends with a newline
    Position;
reportA([<<"forward ", Move/binary>>|Rest], State=#stateA{x=X}) ->
    reportA(Rest, State#stateA{x=X + binary_to_integer(Move)});
reportA([<<"down ", Move/binary>>|Rest], State=#stateA{depth=Depth}) ->
    reportA(Rest, State#stateA{depth=Depth + binary_to_integer(Move)});
reportA([<<"up ", Move/binary>>|Rest], State=#stateA{depth=Depth}) ->
    reportA(Rest, State#stateA{depth=Depth - binary_to_integer(Move)});
reportA([], Position) ->
    Position.


-record(stateB, {x, depth, aim}).

solveB() ->
    {ok, Data} = file:read_file("puzzles/puzzle02-input.txt"),
    solveB(string:split(Data, <<"\n">>, all)).

solveB(Commands) ->
    reportB(Commands, #stateB{x=0, depth=0, aim=0}).

reportB([<<>>], Position) ->
    %% file ends with a newline
    Position;
reportB([<<"forward ", Move/binary>>|Rest],
        State=#stateB{x=X, depth=Depth, aim=Aim}) ->
    M = binary_to_integer(Move),
    reportB(Rest, State#stateB{x=X + M, depth=Depth + Aim * M});
reportB([<<"down ", Move/binary>>|Rest], State=#stateB{aim=Aim}) ->
    reportB(Rest, State#stateB{aim=Aim + binary_to_integer(Move)});
reportB([<<"up ", Move/binary>>|Rest], State=#stateB{aim=Aim}) ->
    reportB(Rest, State#stateB{aim=Aim - binary_to_integer(Move)});
reportB([], Position) ->
    Position.
