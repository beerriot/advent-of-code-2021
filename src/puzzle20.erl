%% Puzzle:
%%
%% Image
%% https://adventofcode.com/2021/day/20
%%
%% explanation:
%% https://blog.beerriot.com/2021/12/20/advent-of-code-day-20/

-module(puzzle20).

-export([
         solveA/0,
         solveB/0,
         load_file/0,
         load_example/0,
         enhance/1,
         display/1,
         count_on_pixels/1
        ]).

solveA() ->
    count_on_pixels(
      enhance(
        enhance(
          load_file()))).

solveB() ->
    count_on_pixels(
      lists:foldl(fun(_, I) -> enhance(I) end,
                  load_file(),
                  lists:seq(1, 50))).

load_file() ->
    {ok, Data} = file:read_file("puzzles/puzzle20-input.txt"),
    parse_input(Data).

load_example() ->
    {ok, Data} = file:read_file("puzzles/puzzle20-example.txt"),
    parse_input(Data).

-record(i, {e,   % enhancement
            w,   % width
            h,   % height
            b,   % background
            d}). % data

parse_input(Data) ->
    [Enhancement,<<>>|ImageLinesPlus] = string:split(Data, <<"\n">>, all),
    %% get rid of trailing newline
    ImageLines = lists:droplast(ImageLinesPlus),
    #i{e=hashdot_to_onezero(Enhancement),
       w=size(hd(ImageLines)),
       h=length(ImageLines),
       b=$0,
       d=hashdot_to_onezero(iolist_to_binary(ImageLines))}.

hashdot_to_onezero(Bin) ->
    << <<(case B of $# -> $1; $. -> $0 end)>> || <<B>> <= Bin >>.

onezero_to_hashdot(Bin) ->
    << <<(case B of $1 -> $#; $0 -> $. end)>> || <<B>> <= Bin >>.

enhance(Image) ->
    enhance(Image, -1, -1, []).

enhance(Image, X, Y, Acc) when X < Image#i.w+1 ->
    enhance(Image, X+1, Y, [enhanced_pixel(Image, X, Y)|Acc]);
enhance(Image, _, Y, Acc) when Y < Image#i.h+1 ->
    enhance(Image, -1, Y+1, Acc);
enhance(Image=#i{w=W,h=H}, _, _, Acc) ->
    Image#i{w=W+2, h=H+2, b=new_background(Image),
            d=list_to_binary(lists:reverse(Acc))}.

enhanced_pixel(Image=#i{e=En}, X, Y) ->
    binary:at(En, enhancement_index(Image, X, Y)).

enhancement_index(Image, X, Y) ->
    list_to_integer([pixel_at(Image, C, R)
                     || {C, R} <- [{X-1, Y-1}, {X, Y-1}, {X+1, Y-1},
                                   {X-1, Y}, {X, Y}, {X+1, Y},
                                   {X-1, Y+1}, {X, Y+1}, {X+1, Y+1}]],
                    2).

pixel_at(#i{d=D, w=W, h=H}, X, Y) when X >= 0, X < W, Y >= 0, Y < H ->
    binary:at(D, Y*W+X);
pixel_at(#i{b=B}, _, _) ->
    B.

new_background(#i{e=En, b=B}) ->
    binary:at(En, list_to_integer(lists:duplicate(9, B), 2)).

display(Image) ->
    display(Image, 0).

display(I=#i{w=W, h=H, d=D}, R) when R < H ->
    <<_:(R*W)/binary, Row:W/binary, _/binary>> = D,
    io:format("~p~n", [onezero_to_hashdot(Row)]),
    display(I, R+1);
display(_, _) ->
    done.

count_on_pixels(#i{d=D}) ->
    lists:sum([ 1 || <<$1>> <= D ]).
