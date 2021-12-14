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
         solveB/0,
         polymerize_count/3
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
     maps:from_list([ {[PA,PB],I} || <<PA,PB," -> ",I>> <- RuleLines ])}.

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
    lists:foldl(fun add_count/2, #{}, Polymer).

add_count(Element, Map) ->
    maps:update_with(Element, fun(V) -> V + 1 end, 1, Map).

solveB() ->
    {Template, Rules} = load_file(),
    Counts = lists:keysort(
               2, maps:to_list(polymerize_count(40, Rules, Template))),
    element(2, hd(lists:reverse(Counts))) - element(2, hd(Counts)).

init_memo(Rules) ->
    maps:from_list([ {{A,B,0}, #{}} || [A,B] <- maps:keys(Rules) ]).

polymerize_count(Steps, Rules, Polymer) ->
    polymerize_count(Steps, Rules, Polymer, init_memo(Rules), #{}).

polymerize_count(Steps, Rules, [A,B|Polymer], Memo, Count) ->
    NewMemo = polymerize_memo(Steps, Rules, [A,B], Memo),
    #{{A,B,Steps} := AddCount} = NewMemo,
    NewCount = maps:merge_with(fun(_, AC, BC) -> AC + BC end,
                               AddCount, Count),
    polymerize_count(Steps, Rules, [B|Polymer], NewMemo,
                     add_count(A, NewCount));
polymerize_count(_, _, [Last], _, Count) ->
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
