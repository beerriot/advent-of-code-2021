%% Puzzle:
%%
%% First half: Add "forward" directives, and net up-down directives.


-module(puzzle02).

-export([
         solve/0,
         solve/1
        ]).

-record(state, {x, depth}).


solve() ->
    solve("puzzles/puzzle02-input.txt").

solve(Filename) ->
    {ok, Data} = file:read_file(Filename),
    Commands = string:split(Data, <<"\n">>, all),
    report(Commands, #state{x=0, depth=0}).

report([<<>>], Position) ->
    %% file ends with a newline
    Position;
report([<<"forward ", Move/binary>>|Rest], State=#state{x=X}) ->
    report(Rest, State#state{x=X + binary_to_integer(Move)});
report([<<"down ", Move/binary>>|Rest], State=#state{depth=Depth}) ->
    report(Rest, State#state{depth=Depth + binary_to_integer(Move)});
report([<<"up ", Move/binary>>|Rest], State=#state{depth=Depth}) ->
    report(Rest, State#state{depth=Depth - binary_to_integer(Move)});
report([], Position) ->
    Position.
