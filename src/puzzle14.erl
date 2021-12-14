%% Puzzle:
%%
%% transparent paper folding
%% https://adventofcode.com/2021/day/14
%%
%% explanation:
%% https://blog.beerriot.com/2021/12/14/advent-of-code-day-14/

-module(puzzle14).

-export([
         solveA/0,
         parse_input/1,
         polymerize/3,
         count_elements/1,
         polymerizeB/4,
         solveB/0,
         init_memo/1,
         polymerizeC/5
        ]).

solveA() ->
    {Template, Rules} = load_file(),
    Polymer = polymerize(10, Rules, Template),
    Counts = lists:keysort(2, maps:to_list(count_elements(Polymer))),
    element(2, hd(lists:reverse(Counts))) - element(2, hd(Counts)).

load_file() ->
    {ok, Data} = file:read_file("puzzles/puzzle14-input.txt"),
    parse_input(Data).

parse_input(Data) ->
    {[Template], [<<>>|RuleLines]} =
        lists:splitwith(fun(L) -> L =/= <<>> end,
                        string:split(Data, <<"\n">>, all)),
    {binary_to_list(Template),
     lists:foldl(fun(<<PA,PB," -> ",I>>, Rules) ->
                         Rules#{[PA,PB] => I};
                    (<<>>, Rules) ->
                         %% ignore final newline
                         Rules
                 end,
                 #{},
                 RuleLines)}.

polymerize(0, _, Polymer) ->
    Polymer;
polymerize(Count, Rules, Polymer) ->
    polymerize(Count-1, Rules, polymerize_step(Rules, Polymer, [])).

polymerize_step(_, [Last], Acc) ->
    lists:reverse([Last|Acc]);
polymerize_step(Rules, [A,B|Template], Acc) ->
    #{[A,B] := I} = Rules,
    polymerize_step(Rules, [B|Template], [I,A|Acc]).

count_elements(Polymer) ->
    lists:foldl(fun(E, Count) ->
                        case Count of
                            #{E := C} ->
                                Count#{E := C+1};
                            _ ->
                                Count#{E => 1}
                        end
                end,
                #{},
                Polymer).

polymerizeB(Steps, Rules, [A,B|Polymer], Counts) ->
    NewCounts = case Counts of
                    #{A := ACount} ->
                        Counts#{A := ACount + 1};
                    _ ->
                        Counts#{A => 1}
                end,
    polymerizeB(Steps, Rules, [B|Polymer],
                polymerize_depth(Steps, Rules, [A,B], NewCounts));
polymerizeB(_, _, [Last], Counts) ->
    case Counts of
        #{Last := LastCount} ->
            Counts#{Last := LastCount + 1};
        _->
            Counts#{Last := 1}
    end.

polymerize_depth(0, _, _, Counts) ->
    Counts;
polymerize_depth(Steps, Rules, [A,B], Counts) ->
    #{[A,B] := I} = Rules,
    case polymerize_depth(Steps-1, Rules, [A,I],
                          polymerize_depth(Steps-1, Rules, [I,B], Counts)) of
        NewCount=#{I := ICount} ->
            NewCount#{I := ICount + 1};
        NewCount ->
            NewCount#{I => 1}
    end.

init_memo(Rules) ->
    maps:from_list([ {{A,B,0}, #{}} || [A,B] <- maps:keys(Rules) ]).

solveB() ->
    {Template, Rules} = load_file(),
    Counts = lists:keysort(2, maps:to_list(polymerizeC(40, Rules, Template, init_memo(Rules), #{}))),
    element(2, hd(lists:reverse(Counts))) - element(2, hd(Counts)).

polymerizeC(Steps, Rules, [A,B|Polymer], Memo, Count) ->
    NewMemo = polymerize_memo(Steps, Rules, [A,B], Memo),
    #{{A,B,Steps} := AddCount} = NewMemo,
    NewCount = maps:merge_with(fun(_, AC, BC) -> AC + BC end,
                               AddCount, Count),
    polymerizeC(Steps, Rules, [B|Polymer], NewMemo, add_count(A, NewCount));
polymerizeC(_, _, [Last], _, Count) ->
    add_count(Last, Count).

polymerize_memo(0, _, _, Memo) ->
    Memo;
polymerize_memo(Steps, Rules, [A,B], Memo) ->
    case Memo of
        #{{A,B,Steps} := _} ->
            Memo;
        _ ->
           #{[A,B] := I} = Rules,
           NewMemo =
                polymerize_memo(Steps-1, Rules, [A,I],
                                polymerize_memo(Steps-1, Rules, [I,B], Memo)),
            #{{A,I,Steps-1} := AICount,
              {I,B,Steps-1} := IBCount} = NewMemo,
            NewCount = maps:merge_with(fun(_, AC, BC) -> AC + BC end,
                                       AICount, IBCount),
            NewMemo#{{A,B,Steps} => add_count(I, NewCount)}
    end.


add_count(Element, Map) ->
    maps:update_with(Element, fun(V) -> V + 1 end, 1, Map).
