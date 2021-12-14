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
         count_elements/1
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
