%% Puzzle:
%%
%% ALU
%% https://adventofcode.com/2021/day/24

-module(puzzle24).

-compile([export_all]).

load_file() ->
    {ok, Data} = file:read_file("puzzles/puzzle24-input.txt"),
    parse_input(Data).

parse_input(Data) ->
    [ parse_line(L) || L <- string:split(Data, <<"\n">>, all),
                       L =/= <<>> ].

parse_line(<<"inp ", Reg>>) ->
    {inp, reg_name(Reg)};
parse_line(<<Op:3/binary, " ", Reg, " ", Arg/binary>>) ->
    {op_name(Op), reg_name(Reg), argument(Arg)}.

op_name(<<"add">>) -> add;
op_name(<<"mul">>) -> mul;
op_name(<<"div">>) -> dib;
op_name(<<"mod">>) -> mod;
op_name(<<"eql">>) -> eql;
op_name(<<"inp">>) -> inp.

reg_name($w) -> w;
reg_name($x) -> x;
reg_name($y) -> y;
reg_name($z) -> z.

argument(<<C>>) when C >= $w, C =< $z ->
    reg_name(C);
argument(Number) ->
    binary_to_integer(Number).

-record(reg, {w=0, x=0, y=0, z=0}).

reg_num(w) -> #reg.w;
reg_num(x) -> #reg.x;
reg_num(y) -> #reg.y;
reg_num(z) -> #reg.z.

set_reg(Reg, State, Val) ->
    setelement(reg_num(Reg), State, Val).

get_reg(Reg, State) ->
    element(reg_num(Reg), State).

get_val(Reg, State) when is_atom(Reg) ->
    get_reg(Reg, State);
get_val(Val, _State) ->
    Val.

exec(Instructions, Inputs) ->
    exec(Instructions, #reg{}, Inputs).

exec([{inp, Reg}|Rest], State, [I|Input]) ->
    exec(Rest, set_reg(Reg, State, I), Input);
exec([{Op, Reg, Arg}|Rest], State, Input) ->
    Val1 = get_reg(Reg, State),
    Val2 = get_val(Arg, State),
    exec(Rest, set_reg(Reg, State, apply_op(Op, Val1, Val2)), Input);
exec([], State, _Input) ->
    State.

apply_op(add, A, B) -> A + B;
apply_op(mul, A, B) -> A * B;
apply_op(dib, A, B) -> A div B;
apply_op(mod, A, B) -> A rem B;
apply_op(eql, A, B) ->
    case A == B of
        true -> 1;
        false -> 0
    end.
