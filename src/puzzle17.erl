%% Puzzle:
%%
%% transparent paper folding
%% https://adventofcode.com/2021/day/17
%%
%% Initial A solution: no code needed

%%  - highest Y velocity for target below start is abs(lower_end)-1,
%%    because it will trace same steps up and down, and then makes one
%%    more step, which needs to hit bottom end
%%
%%  - highest Y reached during that point is (Y * (Y+1))/2

%% Solve B: length(lists:usort(puzzle17:find_solutions(MinX, MaxX, MinY, MaxY))).

-module(puzzle17).

-export([
         solveA/0
        ]).

-compile([export_all]).

solveA() -> see_comment.

%% Notes for Part 2:
%%
%% - Solve X first. That will tell you what steps will be in the
%%   horizontal target region for that value X.
%%
%% - Special cases: Probes that reach X=0 velocity within the target
%%   stay in the target (horizontally) from that point onward.
%%
%% - Start with max Y velocity, and work downward, calculating Y
%%   steps that meet the target, and then cross check with X.
%%
%% - Stop with the first Y that has a step below the target earlier
%%   than any X step in the target.

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
