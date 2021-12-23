%% Puzzle:
%%
%% Octopi
%% https://adventofcode.com/2021/day/11
%%
%% explanation:
%% https://blog.beerriot.com/2021/12/23/advent-of-code-day-11/

-module(puzzle11).

-export([
         solveA/0,
         solveB/0,
         load_octopi/0,
         count_flashes_for_round/2,
         count_rounds_to_all_flash/2,
         octopus/2
        ]).

solveA() ->
    count_flashes_for_round(100, load_octopi()).

solveB() ->
    count_rounds_to_all_flash(1000, load_octopi()).

load_octopi() ->
    {ok, Data} = file:read_file("puzzles/puzzle11-input.txt"),
    [ [ C - $0 || C <- binary_to_list(Row) ]
      || Row <- string:split(Data, <<"\n">>, all), Row =/= <<>> ].

count_flashes_for_round(Rounds, Octopi) ->
    Pids = start_octopi(Octopi),
    _ = send_neighbors(Pids),
    observer_steps(Rounds, lists:flatten(Pids)).

count_rounds_to_all_flash(MaxRounds, Octopi) ->
    Pids = start_octopi(Octopi),
    _ = send_neighbors(Pids),
    observer_all_flash(MaxRounds, lists:flatten(Pids), 0).

start_octopi(Octopi) ->
    Pids = [ [new_octopus(E) || E <- Row] || Row <- Octopi ],
    wait_for_ready(lists:flatten(Pids)),
    Pids.

wait_for_ready([]) ->
    ok;
wait_for_ready(Pids) ->
    receive {ready, P} -> wait_for_ready(lists:delete(P, Pids)) end.

new_octopus(Energy) ->
    spawn(puzzle11, octopus, [self(), Energy]).

send_neighbors(Pids) ->
    [ pid_at(C, R, Pids) ! {neighbors, [pid_at(X, Y, Pids)
                                        || X <- [C-1, C, C+1],
                                           Y <- [R-1, R, R+1],
                                           X > 0,
                                           X =< length(hd(Pids)),
                                           Y > 0,
                                           Y =< length(Pids),
                                           {X,Y} /= {C,R}]}
      || R <- lists:seq(1, length(Pids)),
         C <- lists:seq(1, length(hd(Pids))) ].

pid_at(X, Y, Pids) ->
    lists:nth(X, lists:nth(Y, Pids)).

octopus(Observer, Energy) ->
    Observer ! {ready, self()},
    receive {neighbors, Neighbors} ->
            octopus(Observer, Neighbors, 0, Energy, false)
    end.

octopus(Observer, Neighbors, Step, Energy, Flashed) ->
    receive
        {Step, advance} ->
            case Energy + 1 of
                10 ->
                    _ = [ N ! {Step+1, {flash, self()}} || N <- Neighbors ],
                    octopus(Observer, Neighbors, Step+1, 0,
                            {self, length(Neighbors), 1});
                NewEnergy ->
                    Observer ! {Step+1, {done, 0}},
                    octopus(Observer, Neighbors, Step+1, NewEnergy, false)
            end;
        {Step, {flash, Neighbor}} ->
            case Flashed of
                {_, _, _} ->
                    Neighbor ! {Step, {reflect, 0}},
                    octopus(Observer, Neighbors, Step, Energy, Flashed);
                false ->
                    case Energy + 1 of
                        10 ->
                            _ = [ N ! {Step, {flash, self()}}
                                  || N <- Neighbors ],
                            octopus(Observer, Neighbors, Step, 0,
                                    {{reflect,Neighbor},
                                     length(Neighbors),
                                     1});
                        NewEnergy ->
                            Neighbor ! {Step, {reflect, 0}},
                            octopus(Observer, Neighbors, Step, NewEnergy,
                                    false)
                    end
            end;
        {Step, {reflect, Reflects}} ->
            case Flashed of
                {self, 1, Flashes} ->
                    Observer ! {Step, {done, Flashes+Reflects}},
                    octopus(Observer, Neighbors, Step, Energy,
                            {self, 0, Flashes+Reflects});
                {{reflect, Neighbor}, 1, Flashes} ->
                    Neighbor ! {Step, {reflect, Flashes+Reflects}},
                    octopus(Observer, Neighbors, Step, Energy,
                            {{reflect, Neighbor}, 0, Flashes+Reflects});
                {Who, Remaining, Flashes} ->
                    octopus(Observer, Neighbors, Step, Energy,
                            {Who, Remaining - 1, Flashes + Reflects})
            end;
        stop ->
            ok
    end.

observer_steps(Steps, Pids) ->
    Flashes = lists:foldl(
                fun(Step, Flashes) ->
                        NewStep = observer_advance(Pids, Step),
                        observer_wait(Pids, NewStep, Flashes, length(Pids))
                end,
                0,
                lists:seq(0, Steps-1)),
    _ = [P ! stop || P <- Pids],
    Flashes.

observer_all_flash(MaxSteps, Pids, MaxSteps) ->
    _ = [P ! stop || P <- Pids],
    not_observed;
observer_all_flash(MaxSteps, Pids, Step) ->
    NewStep = observer_advance(Pids, Step),
    case observer_wait(Pids, NewStep, 0, length(Pids)) of
        Flashes when Flashes == length(Pids) ->
            _ = [P ! stop || P <- Pids],
            NewStep;
        _ ->
            observer_all_flash(MaxSteps - 1, Pids, NewStep)
    end.

observer_advance(Pids, Step) ->
    _ = [ P ! {Step, advance} || P <- Pids ],
    Step + 1.

observer_wait(Pids, Step, Flashes, Advance) ->
    receive
        {Step, {done, NewFlashes}} ->
            case Advance - 1 of
                0 ->
                    Flashes + NewFlashes;
                NewAdvance ->
                    observer_wait(Pids, Step, Flashes+NewFlashes, NewAdvance)
            end
    end.
