%% Puzzle:
%%
%% transparent paper folding
%% https://adventofcode.com/2021/day/16

-module(puzzle16).

-export([
         solveA/0,
         hex_to_bin/1,
         parse_packet/1,
         parse_all_packets/1,
         sum_versions/1
        ]).

solveA() ->
    {Packet, _} = parse_packet(load_file()),
    sum_versions(Packet).

load_file() ->
    {ok, HexData} = file:read_file("puzzles/puzzle16-input.txt"),
    hex_to_bin(HexData).

hex_to_bin(HexData) ->
    BinData = << <<(binary_to_integer(B, 16))>>
                 || <<B:2/binary>> <= HexData >>,
    case size(BinData) * 2 == size(HexData) of
        true ->
            BinData;
        false ->
            case HexData of
                <<_:(size(BinData) * 2)/binary, $\n>> ->
                    BinData;
                <<_:(size(BinData) * 2)/binary, B/binary>> ->
                    <<BinData/binary,
                      (binary_to_integer(B, 16)):4/bitstring>>
            end
    end.

-record(packet, {ver, type, value, subs}).

parse_packet(<<Ver:3, 4:3, Rest/bitstring>>) ->
    {Literal, RestB} = parse_literal(Rest),
    {#packet{ver=Ver, type=literal, value=Literal}, RestB};
parse_packet(<<Ver:3, _Type:3, 0:1, BitLength:15, Rest/bitstring>>) ->
    <<SubPacketData:BitLength/bitstring, RestB/bitstring>> = Rest,
    Packets = parse_all_packets(SubPacketData),
    {#packet{ver=Ver, type=operator, subs=Packets}, RestB};
parse_packet(<<Ver:3, _Type:3, 1:1, SubCount:11, Rest/bitstring>>) ->
    {Packets, RestB} = parse_n_packets(SubCount, Rest),
    {#packet{ver=Ver, type=operator, subs=Packets}, RestB};
parse_packet(Rest) ->
    {'end', Rest}.

parse_literal(<<0:1, N:4, Rest/bitstring>>) -> {N, Rest};
parse_literal(Extended) -> parse_literal(Extended, 0).

parse_literal(<<1:1, N:4, Rest/bitstring>>, Acc) ->
    parse_literal(Rest, (Acc bsl 4) + N);
parse_literal(<<0:1, N:4, Rest/bitstring>>, Acc) ->
    {(Acc bsl 4) + N, Rest}.

parse_all_packets(BinData) ->
    case parse_packet(BinData) of
        {'end', _} ->
            [];
        {Packet, Rest} ->
            [Packet|parse_all_packets(Rest)]
    end.

parse_n_packets(N, BinData) ->
    {RevPackets, Rest} =
        lists:foldl(fun(_, {Acc, Data}) ->
                            {Packet, Rest} = parse_packet(Data),
                            {[Packet|Acc], Rest}
                    end,
                    {[], BinData},
                    lists:seq(1, N)),
    {lists:reverse(RevPackets), Rest}.

sum_versions(#packet{ver=V, subs=undefined}) ->
    V;
sum_versions(#packet{ver=V, subs=Subs}) ->
    V + lists:sum([sum_versions(S) || S <- Subs]).
