%% Puzzle:
%%
%% Fish

-module(puzzle06).

-export([
         solveA/0,
         solveB/0,
         model_fishA/2,
         model_fishB/2,
         model_fishB/3
        ]).

solveA() ->
    model_fishA(80, load_fish()).

solveB() ->
    model_fishB(256, load_fish()).

load_fish() ->
    {ok, Data} = file:read_file("puzzles/puzzle06-input.txt"),
    [OneLine|_] = string:split(Data, <<"\n">>),
    [ binary_to_integer(F) ||
        F <- string:split(OneLine, <<",">>, all) ].

model_fishA(0, Fish) ->
    Fish;
model_fishA(Days, Fish) ->
    model_fishA(
      Days-1,
      lists:foldl(fun(0, Acc) -> [8, 6 | Acc];
                     (F, Acc) -> [F-1 | Acc]
                  end,
                  [], Fish)).

model_fishB(Days, Fish) ->
    BDays = [ F-6 || F <- Fish ],
    Table = model_fishB(Days, BDays, []),
    lists:sum([ proplists:get_value(F, Table) || F <- BDays ]).

model_fishB(_, [], Table) ->
    Table;
model_fishB(Days, [Fish|Rest], Table) ->
    case proplists:is_defined(Fish, Table) of
        true ->
            model_fishB(Days, Rest, Table);
        false when Fish >= Days ->
            model_fishB(Days, Rest, [{Fish, 1} | Table]);
        false ->
            Spawn = [ F + 2 || %% first spawn delay
                        F <- lists:seq(Fish+7, Days, 7) ],
            NewTable = model_fishB(Days, Spawn, Table),
            Total = lists:sum([ proplists:get_value(S, NewTable) ||
                                  S <- Spawn ]),
            model_fishB(Days, Rest, [{Fish, Total+1} | NewTable])
    end.
