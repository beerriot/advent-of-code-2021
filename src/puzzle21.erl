%% Puzzle:
%%
%% parsing
%% https://adventofcode.com/2021/day/21

-module(puzzle21).

-export([
        ]).

-compile([export_all]).

-record(p, {    % player
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
