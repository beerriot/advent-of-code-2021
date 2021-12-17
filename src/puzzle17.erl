%% Puzzle:
%%
%% transparent paper folding
%% https://adventofcode.com/2021/day/17
%%
%% explanation:
%% https://blog.beerriot.com/2021/12/17/advent-of-code-day-17/
-module(puzzle17).

-export([
         solveA/0,
         solveB/0,
         parse_input/1,
         find_ys/2,
         find_xs/2,
         find_solutions/4
        ]).

%%  - highest Y velocity for target below start is abs(lower_end)-1,
%%    because it will trace same steps up and down, and then makes one
%%    more step, which needs to hit bottom end
%%
%%  - highest Y reached during that point is (Y * (Y+1))/2
solveA() ->
    [_MinX, _MaxX, MinY, _MaxY] = load_file(),
    Velocity = abs(MinY) - 1,
    (Velocity * (Velocity + 1)) div 2.

solveB() ->
    [MinX, MaxX, MinY, MaxY] = load_file(),
    length(puzzle17:find_solutions(MinX, MaxX, MinY, MaxY)).

load_file() ->
    {ok, Data} = file:read_file("puzzles/puzzle17-input.txt"),
    parse_input(Data).

-define(RE,
        "target area: x=([0-9]+)\\.\\.([0-9]+), y=(-[0-9]+)\\.\\.(-[0-9]+)").
parse_input(Input) ->
    {match, Params} =
        re:run(Input, ?RE, [{capture, [1,2,3,4], binary}]),
    [binary_to_integer(V) || V <- Params ].

%% Notes for Part 2:
%%
%% - Solve X and Y independently: keep a list of which steps for each
%%   velocity end up in the target space, then pair up the two, by
%%   matching the steps. Match X to Y, because of the special case
%%   noted below.
%%
%% - Solving X:
%%    - Skip everything that would have a zero velocity before the target
%%    - Stop searching when the first step of a velocity is past the target
%%    - Special case: some velocities might stop in the target, so mark
%%      that they match every step greater
%%
%% - Solving Y:
%%    - Start with highest positive velocity that hits target
%%    - Work downward until the first velocity has a first step below target

find_ys(Min, Max) ->
    Highest = abs(Min)-1,
    %%            {velocity, step}
    InitPoints = [{Highest, Highest * 2 + 2}],
    find_ys(Min, Max, Highest-1, InitPoints).

find_ys(Min, _Max, Min, Points) ->
    [{Min, 1}|Points];
find_ys(Min, Max, Vel, Points) ->
    case Vel > 0 of
        true ->
            FirstStepBelow0 = Vel * 2 + 2,
            InitVelocityBelow0 = -(Vel+1);
        false ->
            FirstStepBelow0 = 1,
            InitVelocityBelow0 = Vel
    end,
    find_ys(Min, Max, Vel-1,
            [{Vel, Step} || Step <- steps_in_target_y(Min, Max,
                                                      FirstStepBelow0,
                                                      InitVelocityBelow0,
                                                      InitVelocityBelow0-1)]
            ++ Points).

steps_in_target_y(Min, _Max, _Step, Y, _Vel) when Y < Min ->
    [];
steps_in_target_y(Min, Max, Step, Y, Vel) ->
    case Y =< Max of
        true ->
            [Step | steps_in_target_y(Min, Max, Step+1, Y+Vel, Vel-1)];
        false ->
            steps_in_target_y(Min, Max, Step+1, Y+Vel, Vel-1)
    end.

find_xs(Min, Max) ->
    Slowest = find_first_xs(Min, Max, 1),
    [{StartVel,_}|_] = Slowest,
    find_xs(Min, Max, StartVel, Slowest).


find_first_xs(Min, Max, Vel) ->
    case (Vel * (Vel + 1)) div 2 of
        In when Min =< In, In =< Max ->
            [{Vel, Step} || Step <- steps_in_target_x(Min, Max, 1, Vel, Vel-1)];
        In when In > Max ->
            {error, unexpected_slowest_too_fast};
        _ ->
            find_first_xs(Min, Max, Vel+1)
    end.

steps_in_target_x(_Min, Max, _Step, X, _Vel) when X > Max ->
    [];
steps_in_target_x(Min, Max, Step, X, 0) ->
    case (Min =< X) and (X =< Max) of
        true ->
            [{all_gte, Step}];
        false ->
            []
    end;
steps_in_target_x(Min, Max, Step, X, Vel) ->
    case X >= Min of
        true ->
            [Step | steps_in_target_x(Min, Max, Step+1, X+Vel, Vel-1)];
        false ->
            steps_in_target_x(Min, Max, Step+1, X+Vel, Vel-1)
    end.

find_xs(_Min, Max, Vel, Points) when Vel > Max ->
    Points;
find_xs(Min, Max, Vel, Points) ->
    find_xs(Min, Max, Vel+1,
            [{Vel, Step} || Step <- steps_in_target_x(Min, Max, 1, Vel, Vel-1)]
            ++ Points).

find_solutions(MinX, MaxX, MinY, MaxY) ->
    Xs = find_xs(MinX, MaxX),
    Ys = find_ys(MinY, MaxY),
    lists:usort(
      lists:foldl(fun({XVel, Step}, Acc) ->
                          [{XVel, YVel}
                           || YVel <- ys_at_step(Step, Ys)]
                              ++ Acc
                  end,
                  [],
                  Xs)).

ys_at_step({all_gte, Step}, Ys) ->
    [ Y || {Y, S} <- Ys, S >= Step ];
ys_at_step(Step, Ys) ->
    [ Y || {Y, S} <- Ys, S == Step ].
