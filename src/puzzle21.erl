%% Puzzle:
%%
%% parsing
%% https://adventofcode.com/2021/day/21

-module(puzzle21).

-export([
        ]).

-compile([export_all]).

-record(p, {    % player
             i, % index
             p, % position
             s  % score
           }).

-record(d, {    % die
             n, % next roll
             c  % count of rolls
           }).

play_game(P1Start, P2Start) ->
    play_game(#p{p=P1Start, s=0}, #p{p=P2Start, s=0}, #d{n=1, c=0}).

play_game(Player1, Player2, Die) ->
    case player_roll(Player1, Die) of
        {NewPlayer1, Die1} when NewPlayer1#p.s >= 1000 ->
            Player2#p.s * Die1#d.c;
        {NewPlayer1, Die1}->
            case player_roll(Player2, Die1) of
                {NewPlayer2, Die2} when NewPlayer2#p.s >= 1000 ->
                    NewPlayer1#p.s * Die2#d.c;
                {NewPlayer2, Die2} ->
                    play_game(NewPlayer1, NewPlayer2, Die2)
            end
    end.

player_roll(#p{p=Start, s=Score}, Die) ->
    {Move1, Die1} = die_roll(Die),
    {Move2, Die2} = die_roll(Die1),
    {Move3, Die3} = die_roll(Die2),
    Move = Move1+Move2+Move3,
    NewPosition = case (Start + Move) rem 10 of
                      0 -> 10;
                      P -> P
                  end,
    {#p{p=NewPosition, s=Score+NewPosition}, Die3}.

die_roll(#d{n=N, c=C}) ->
    {N, #d{n=case (N + 1) of 101 -> 1; V -> V end, c=C+1}}.

%% 4 -1 5(5) -1 6(11) -1 7(18) -1 8(26)W
%%                             -2 9(27)W
%%                             -3 10(28)W
%%                    -2 8(19) -1 9(28)W
%%                             -2 10(29)W
%%                             -3 1(20) -1 2(22)W
%%                                      -2 3(23)W
%%                                      -3 4(24)W
%%                    -3 9(20) -1 10(30)W
%%                             -2 1(21)W
%%                             -3 2(22)W
%%           -2 7(12) -1 8(20) -1 9(29)W
%%                             -2 10(30)W
%%                             -3 1(21)W
%%                    -2 9(21)W
%%                    -3 10(22)W
%%           -3 8(13) -1 9(22)W
%%                    -2 10(23)W
%%                    -3 1(14) -1 2(16)
%%                             -2 3(17)
%%                             -3 4(18)
%%   -2 6(6) -1 7(13) -1 8(21)W
%%                    -2 9(22)W
%%                    -3 10(23)W
%%           -2 8(14) -1 9(23)W
%%                    -2 10(24)W
%%                    -3 1(15)
%%           -3 9(15) -1 10(25)W
%%                    -2 1(16)
%%                    -3 2(17)
%%   -3 7(7) -1 8(15) -1 9(24)W
%%                    -2 10(25)W
%%                    -3 1(16)
%%           -2 9(16) -1 10(26)W
%%                    -2 1(17)
%%           -3 10(17)-1 1(18)
%%                    -2 2(19)
%%                    -3 3(20)

%% 111 = 3 2xx 4 3xx 5
%% 112 = 4     5     6
%% 113 = 5     6     7
%% 121 = 4     5     6
%% 122 = 5     6     7
%% 123 = 6     7     8
%% 131 = 5     6     7
%% 132 = 6     7     8
%% 133 = 7     8     9

%% There are 27 ways the dice could roll, but sum is fewer:
%% 1 x 3
%% 3 x 4
%% 6 x 5
%% 7 x 6
%% 6 x 7
%% 3 x 8
%% 1 x 9

%%              roll value V  V ways that roll can show up
-define(ALL_ROLL_COUNTS, [{3, 1},
                          {4, 3},
                          {5, 6},
                          {6, 7},
                          {7, 6},
                          {8, 3},
                          {9, 1}]).

dirac_player_win_histo(Start) ->
    dirac_player_win_histo(Start, 0, 0, 1, ?ALL_ROLL_COUNTS, #{}).

dirac_player_win_histo(_Space, _Score, _, _, [], Histo) ->
    Histo;
dirac_player_win_histo(Space, Score, RollCount, LeafMult,
                       [{Roll, RollMult}|Rest], Histo) ->
    NewSpace = case (Space + Roll) rem 10 of 0 -> 10; S -> S end,
    case Score + NewSpace of
        Win when Win > 21 ->
            dirac_player_win_histo(
              Space, Score, RollCount, LeafMult, Rest,
              maps:update_with(RollCount+1,
                               fun(C) -> C+(RollMult*LeafMult) end,
                               RollMult*LeafMult,
                               Histo));
        NewScore ->
            NewHisto = dirac_player_win_histo(NewSpace, NewScore, RollCount+1,
                                              LeafMult*RollMult,
                                              ?ALL_ROLL_COUNTS, Histo),
            dirac_player_win_histo(Space, Score, RollCount, LeafMult, Rest,
                                   NewHisto)
    end.

player_wins(P1Histo, P2Histo) ->
    maps:fold(fun(P1Rolls, P1GameCount, Acc) ->
                      P2Wins = lists:sum([maps:get(K, P2Histo)
                                          || K <- maps:keys(P2Histo),
                                             K < P1Rolls]),
                      Acc + P1GameCount - P2Wins
              end,
              0,
              P1Histo).

dirac_game(P1Start, P2Start) ->
    dirac_game([#p{i=1, p=P1Start, s=0}, #p{i=2, p=P2Start, s=0}], 1, {0, 0}).

dirac_game([Up,Next], LeafMult, Wins) ->
    lists:foldl(fun({Roll, RollMult}, AccWins) ->
                        NP = case (Up#p.p + Roll) rem 10 of 0 -> 10; S -> S end,
                        case Up#p.s + NP of
                            Win when Win >= 21 ->
                                setelement(Up#p.i, AccWins,
                                           LeafMult*RollMult
                                           +element(Up#p.i, AccWins));
                            NS ->
                                dirac_game([Next,Up#p{p=NP,s=NS}],
                                           LeafMult*RollMult,
                                           AccWins)
                        end
                end,
                Wins,
                ?ALL_ROLL_COUNTS).

dirac_game_parallel(P1Start, P2Start) ->
    Me = self(),
    Pids = lists:map(
             fun(Roll) ->
                     spawn(puzzle21, dirac_game_worker,
                           [P1Start, Roll, P2Start, Me])
             end,
             ?ALL_ROLL_COUNTS),
    dirac_game_collector(Pids, {0,0}).

dirac_game_worker(P1Start, {Roll, RollMult}, P2Start, Collector) ->
    NP = case (P1Start + Roll) rem 10 of 0 -> 10; S -> S end,
    Result = dirac_game([#p{i=2, p=P2Start, s=0}, #p{i=1, p=NP, s=NP}],
                        RollMult, {0,0}),
    Collector ! {self(), Result}.

dirac_game_collector([], Wins) ->
    Wins;
dirac_game_collector(Workers, {P1Wins, P2Wins}) ->
    receive {Pid, {P1WorkerWins, P2WorkerWins}} ->
            dirac_game_collector(
              lists:delete(Pid, Workers),
              {P1Wins + P1WorkerWins, P2Wins + P2WorkerWins})
    end.

dirac_game_memoize(P1Start, P2Start) ->
    Players = [#p{i=1, p=P1Start, s=0}, #p{i=2, p=P2Start, s=0}],
    {Wins, _} = dirac_game_memoize_i(Players, #{}),
    Wins.

dirac_game_memoize_i([Up,Next], Memo) ->
    case Memo of
        #{[Up,Next] := Wins} -> {Wins, Memo};
        _ ->
            {Wins, NewMemo} =
                lists:foldl(
                  fun({Roll, RollMult}, {AccWins, AccMemo}) ->
                          NP = case (Up#p.p + Roll) rem 10
                               of 0 -> 10; S -> S end,
                          case Up#p.s + NP of
                              Win when Win >= 21 ->
                                  {setelement(Up#p.i, AccWins,
                                              RollMult
                                              +element(Up#p.i, AccWins)),
                                   %% don't bother memoizing leaves -
                                   %% recomputing them is cheap
                                   AccMemo};
                              NS ->
                                  SubPlayers = [Next,Up#p{p=NP,s=NS}],
                                  {{P1SubWins,P2SubWins},SubMemo} =
                                      dirac_game_memoize_i(SubPlayers,
                                                           AccMemo),
                                  {{RollMult*P1SubWins+element(1, AccWins),
                                    RollMult*P2SubWins+element(2, AccWins)},
                                   SubMemo}
                          end
                  end,
                  {{0,0}, Memo},
                  ?ALL_ROLL_COUNTS),
            {Wins, NewMemo#{[Up,Next] => Wins}}
    end.
