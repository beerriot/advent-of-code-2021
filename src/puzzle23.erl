%% Puzzle:
%%
%% Octopi
%% https://adventofcode.com/2021/day/23

-module(puzzle23).

-export([
        ]).

-compile([export_all]).

%% use these, as they are the distance from the hallway
-define(NONROOM_HALLWAYS, [0,1,3,5,7,9,10]).

distance({hallway, H1}, {hallway, H2}) ->
    abs(H1-H2);
distance({room, R, TB}, {hallway, H}) ->
    TB + distance({hallway, hallway_outside_room(R)}, {hallway, H});
distance({hallway,_}=H, {room, _, _}=R) ->
    distance(R, H);
distance({room, R, TB1}, {room, R, TB2}) ->
    abs(TB1-TB2);
distance({room, R1, TB1}, {room, R2, TB2}) ->
    TB1 + TB2 + distance({hallway, hallway_outside_room(R1)},
                         {hallway, hallway_outside_room(R2)}).

hallway_outside_room(a) -> 2;
hallway_outside_room(b) -> 4;
hallway_outside_room(c) -> 6;
hallway_outside_room(d) -> 8;
hallway_outside_room({room, Type, _}) -> hallway_outside_room(Type);
hallway_outside_room(_) -> false.

room_at_hallway(2) -> a;
room_at_hallway(4) -> b;
room_at_hallway(6) -> c;
room_at_hallway(8) -> d;
room_at_hallway({hallway, H}) -> room_at_hallway(H);
room_at_hallway(_) -> false.

-record(pod, {
              t, % type: a,b,c,d
              l  % location: {hallway, N}, {room, Type, 1|2}
             }).

cost_per_space(#pod{t=a}) -> 1;
cost_per_space(#pod{t=b}) -> 10;
cost_per_space(#pod{t=c}) -> 100;
cost_per_space(#pod{t=d}) -> 1000.

distance_to_target(#pod{t=Type, l=Loc}, Pods, RoomSize) ->
    case Loc of
        {room, Type, RoomSize} ->
            %% were in the bottom of the right room - no move needed
            0;
        {room, Type, RoomSpot} ->
            case lower_pods_in_correct_room(Type, RoomSpot, RoomSize, Pods) of
                true ->
                    %% under us is the correct type in the right room
                    %% - no move needed
                    0;
                false ->
                    %% some other type is below us, so we'll have to
                    %% move out and back in at least one step
                    RoomSpot + 3
            end;
        _ ->
            %% distance to be in the room at all is the question
            distance(Loc, {room, Type, 1})
    end.

finished(Pods) ->
    lists:all(fun(#pod{t=T, l={room, T, _}}) -> true; (_) -> false end,
              Pods).

estimated_cost_left(Pods, RoomSize) ->
    lists:sum([ cost_per_space(P) * distance_to_target(P, Pods, RoomSize)
                || P <- Pods ]).

legal_moves(#pod{t=Type, l={room, Type, RoomSize}},
            _Others, RoomSize, _Opts) ->
    %% yes it can move, but no we shouldn't bother wasting time
    %% considering it
    [];
legal_moves(#pod{t=Type, l={room, Type, RoomSpot}}=P,
            Others, RoomSize, Opts) ->
    case lower_pods_in_correct_room(Type, RoomSpot, RoomSize, Others) of
        true ->
            %% as with already being in the bottom position, there is
            %% nowhere else we should consider this amphipod going
            %% (because it's in the right room, with only right-room
            %% amphipods below it)
            [];
        false ->
            case can_reach_hallway(Type, RoomSpot, Others) of
                true ->
                    %% go somewhere else
                    legal_hallway_moves(P, Others, RoomSize, Opts);
                false ->
                    %% stuck
                    []
            end
    end;
legal_moves(#pod{l={room, Type, RoomSpot}}=P, Others, RoomSize, Opts) ->
    case can_reach_hallway(Type, RoomSpot, Others) of
        true ->
            %% go somewhere else
            legal_hallway_moves(P, Others, RoomSize, Opts);
        false ->
            %% we're stuck here until the amphipod on the top moves
            []
    end;
legal_moves(Pod, Others, RoomSize, Opts) ->
    %% we're already in the hallway
    legal_hallway_moves(Pod, Others, RoomSize, Opts).

lower_pods_in_correct_room(Type, MinSpot, RoomSize, Pods) ->
    lists:all(fun(Spot) ->
                      case lists:keyfind({room, Type, Spot},
                                         #pod.l, Pods) of
                          #pod{t=OT} ->
                              OT == Type;
                          false ->
                              %% allows search from 0 for hallway search
                              true
                      end
              end,
              lists:seq(MinSpot+1, RoomSize)).

can_reach_hallway(_Type, 1, _Pods) -> true;
can_reach_hallway(Type, Spot, Pods) ->
    case lists:keyfind({room, Type, Spot-1}, #pod.l, Pods) of
        #pod{} ->
            false;
        false ->
            true
    end.

legal_hallway_moves(#pod{t=Type, l={hallway, H}}, Others, RoomSize, _Opts) ->
    Target = hallway_outside_room(Type),
    case lists:any(fun(#pod{l={hallway, H2}}) ->
                           (min(H,Target) < H2) and (H2 < max(H,Target));
                      (_) ->
                           false
                   end,
                   Others) of
        true ->
            %% our way is blocked
            [];
        false ->
            case lower_pods_in_correct_room(Type, 0, RoomSize, Others) of
                false ->
                    %% non-Type amphipods in this room
                    [];
                true ->
                    case [ Spot || #pod{l={room, T, Spot}} <- Others,
                                   T == Type] of
                        [] ->
                            %% no one here - move into bottom
                            [{room, Type, RoomSize}];
                        Used ->
                            %% move in above others
                            [{room, Type, lists:min(Used)-1}]
                    end
            end
    end;
legal_hallway_moves(#pod{l={room, Type, _}}=P, Others, RoomSize, room_only) ->
    Outside = hallway_outside_room(Type),
    legal_hallway_moves(P#pod{l={hallway, Outside}},
                        Others, RoomSize, room_only);
legal_hallway_moves(#pod{l={room, Type, _}}=P, Others, RoomSize, Opts) ->
    %% we came through legal_moves, which verified we're not in the
    %% bottom of a room, with another amphipod in the top
    %%
    %% figure out what hallway spots we can get to
    Outside = hallway_outside_room(Type),
    ReachableHallway = lists:foldl(fun(#pod{l={hallway, H}}, Acc)
                                      when H < Outside ->
                                           %% pod is to the left, only
                                           %% keep spots to the right
                                           [ A || A <- Acc, A > H];
                                      (#pod{l={hallway, H}}, Acc) ->
                                           %% pod is to the right,
                                           %% only keep spots to the
                                           %% left
                                           [ A || A <- Acc, A < H];
                                      (_, Acc) ->
                                           Acc
                                   end,
                                   ?NONROOM_HALLWAYS,
                                   Others),
    [ {hallway, H} || H <- ReachableHallway ]
      %% add options for moving straight through the hallway into the
      %% right room
      ++ legal_hallway_moves(P#pod{l={hallway, Outside}},
                             Others, RoomSize, Opts).

solve(InitPodRooms) ->
    {RoomSize, InitPods} = init_pods(InitPodRooms),
    InitEst = estimated_cost_left(InitPods, RoomSize),
    solve([{0, InitEst, InitPods}], #{InitPods => 0}, RoomSize, 0).

solve_expanded({[A1,A2],[B1,B2],[C1,C2],[D1,D2]}) ->
    solve({[A1,d,d,A2],[B1,c,b,B2],[C1,b,a,C2],[D1,a,c,D2]}).

init_pods({A, B, C, D}) ->
    {hd([_]=lists:usort([length(R) || R <- [A,B,C,D]])),
     make_room(a, A)++make_room(b, B)++make_room(c, C)++make_room(d, D)}.

make_room(Room, Types) ->
    [#pod{t=T, l={room, Room, I}}
     || {T, I} <- lists:zip(Types, lists:seq(1, length(Types))) ].

solve([{BestCost, Est, Best}|Options], Seen, RoomSize, Debug) ->
    case Debug rem 1000 of
        0 ->
            io:format("length(Options) = ~p "
                      "size(Seen) = ~p "
                      "BestCost = ~p "
                      "Total = ~p~n",
                      [length(Options), maps:size(Seen), BestCost,
                       BestCost+Est]);
        _ ->
            ok
    end,
    case Seen of
        #{Best := LowerCost} when LowerCost < BestCost ->
            %% we found a cheaper path to this state since this option
            %% was put in the list
            solve(Options, Seen, RoomSize, Debug+1);
        _ ->
            {RoomCost, RoomOnly} =
                make_room_only_moves(BestCost, Best, RoomSize),
            %% cost comparison s efficiency only: we know Best wasn't finished
            case (RoomCost =/= BestCost) andalso finished(RoomOnly) of
                true ->
                    {RoomCost, RoomOnly};
                false ->
                    NewOptions = all_legal_moves(RoomCost, RoomOnly, RoomSize),
                    {CheapOrNew, NewSeen} =
                        update_and_filter_seen(NewOptions, Seen),
                    solve(lists:merge(fun sort_total_path_cost/2,
                                      lists:sort(fun sort_total_path_cost/2,
                                                 CheapOrNew),
                                      Options),
                          NewSeen, RoomSize, Debug+1)
            end
    end.

make_room_only_moves(StartCost, Start, RoomSize) ->
    case lists:foldl(fun(P, {AccCost, AccState}) ->
                             Without = lists:delete(P, AccState),
                             case legal_moves(P, Without, RoomSize,
                                              room_only) of
                                 [Move] ->
                                     AddCost = cost_per_space(P)
                                         * distance(P#pod.l, Move),
                                     {AddCost+AccCost,
                                      [P#pod{l=Move}|Without]};
                                 [] ->
                                     {AccCost, AccState}
                             end
                     end,
                     {StartCost, Start},
                     Start) of
        {StartCost, _} ->
            {StartCost, Start};
        {NewCost, NewStart} ->
            make_room_only_moves(NewCost, NewStart, RoomSize)
    end.

all_legal_moves(Cost, State, RoomSize) ->
    lists:append(
      [ begin
            Without = lists:delete(P, State),
            Moves = legal_moves(P, Without, RoomSize, all),
            case lists:keyfind(room, 1, Moves) of
                false ->
                    RealMoves = Moves;
                IntoRoom ->
                    io:format("ROOM ONLY DID NOT WORK~n", []),
                    RealMoves = [IntoRoom]
            end,
            [ begin
                  AddCost = cost_per_space(P) * distance(P#pod.l, Move),
                  NewState = [P#pod{l=Move}|Without],
                  Est = estimated_cost_left(NewState, RoomSize),
                  {AddCost+Cost, Est, NewState}
              end
              || Move <- RealMoves ]
        end
        || P <- State ]).

update_and_filter_seen(Options, Seen) ->
    update_and_filter_seen(Options, Seen, []).

update_and_filter_seen([{Cost, _, State}=Opt|Rest], Seen, Keep) ->
    case Seen of
        #{State := Cheaper} when Cheaper =< Cost ->
            update_and_filter_seen(Rest, Seen, Keep);
        _ ->
            update_and_filter_seen(Rest, Seen#{State => Cost}, [Opt|Keep])
    end;
update_and_filter_seen([], Seen, Keep) ->
    {Keep, Seen}.

sort_total_path_cost({ACost, AEst, _}, {BCost, BEst, _}) ->
    (ACost+AEst) =< (BCost+BEst).
