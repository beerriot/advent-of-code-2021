%% Puzzle:
%%
%% https://adventofcode.com/2021/day/18
%%
%% explanation:
%% https://blog.beerriot.com/2021/12/17/advent-of-code-day-18/

-module(puzzle18).

-export([
         solveA/0,
         load_file/0,
         parse_snail_number/1,
         add/2,
         reduce/1,
         magnitude/1,
         largest_magnitude_of/3,
         solveB/0
        ]).

solveA() ->
    [First|Rest] = load_file(),
    magnitude(lists:foldl(fun(B, A) -> puzzle18:add(A, B) end, First, Rest)).

load_file() ->
    {ok, Data} = file:read_file("puzzles/puzzle18-input.txt"),
    [ parse_snail_number(L) || L <- string:split(Data, <<"\n">>, all),
                               L =/= <<>> ].

parse_snail_number(Line) ->
    {Number, <<>>} = parse_snail_number_i(Line),
    Number.

parse_snail_number_i(<<$[, Rest/binary>>) ->
    {Number1, <<$,, RestB/binary>>} = parse_snail_number_i(Rest),
    {Number2, <<$], RestC/binary>>} = parse_snail_number_i(RestB),
    {{Number1, Number2}, RestC};
parse_snail_number_i(<<C, Rest/binary>>) when C >= $0, C =< $9->
    {C-$0, Rest}.

add(N1, N2) -> reduce({N1, N2}).

reduce(N) ->
    case maybe_explode(N, 0) of
        {true, New, _, _} ->
            reduce(New);
        false ->
            case maybe_split(N) of
                {true, New} ->
                    reduce(New);
                false ->
                    N
            end
    end.

maybe_explode({A,B}, Depth) when Depth >= 4, is_integer(A), is_integer(B) ->
    {true, 0, A, B};
maybe_explode({A,B}, Depth) ->
    case maybe_explode(A, Depth+1) of
        {true, New, AddA, AddB} ->
            {true, {New, add_explode({b, AddB}, B)}, AddA, 0};
        false ->
            case maybe_explode(B, Depth+1) of
                {true, New, AddA, AddB} ->
                    {true, {add_explode({a, AddA}, A), New}, 0, AddB};
                false ->
                    false
            end
    end;
maybe_explode(N, _) when is_integer(N) ->
    false.

add_explode({_, Add}, Num) when is_integer(Num) ->
    Add + Num;
add_explode({b, Add}, {A,B}) ->
    {add_explode({b, Add}, A), B};
add_explode({a, Add}, {A,B}) ->
    {A, add_explode({a,Add}, B)}.

maybe_split(N) when is_integer(N) ->
    case N > 9 of
        true ->
            Left = N div 2,
            Right = case Left*2 == N of true -> Left; false -> Left+1 end,
            {true, {Left, Right}};
        false ->
            false
    end;
maybe_split({A, B}) ->
    case maybe_split(A) of
        {true, NewA} ->
            {true, {NewA, B}};
        false ->
            case maybe_split(B) of
                {true, NewB} ->
                    {true, {A, NewB}};
                false ->
                    false
            end
    end.

magnitude(N) when is_integer(N) ->
    N;
magnitude({A, B}) ->
    3 * magnitude(A) + 2 * magnitude(B).

largest_magnitude_of(_, [], Mag) ->
    Mag;
largest_magnitude_of(A, [B|Rest], Mag) ->
    case magnitude(add(A,B)) of
        New when New > Mag ->
            largest_magnitude_of(A, Rest, New);
        _ ->
            largest_magnitude_of(A, Rest, Mag)
    end.

solveB() ->
    Numbers = load_file(),
    lists:foldl(fun(I, Mag) ->
                        {Left, [This|Right]} = lists:split(I, Numbers),
                        largest_magnitude_of(This, Left++Right, Mag)
                end,
                0,
                lists:seq(0, length(Numbers)-1)).
