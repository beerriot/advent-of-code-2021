%% Puzzle:
%%
%% Fish

-module(puzzle06).

-export([
         solveA/0,
         model_fish/2
        ]).

solveA() ->
    {ok, Data} = file:read_file("puzzles/puzzle06-input.txt"),
    [OneLine|_] = string:split(Data, <<"\n">>),
    Fish = [ binary_to_integer(F) ||
               F <- string:split(OneLine, <<",">>, all) ],
    model_fish(80, Fish).

model_fish(0, Fish) ->
    Fish;
model_fish(Days, Fish) ->
    model_fish(
      Days-1,
      lists:foldl(fun(0, Acc) -> [8, 6 | Acc];
                     (F, Acc) -> [F-1 | Acc]
                  end,
                  [], Fish)).
