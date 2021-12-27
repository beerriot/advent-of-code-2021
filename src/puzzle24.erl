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

step(Instructions, Inputs) ->
    step({Instructions, #reg{}, Inputs}).

step({[I|Rest], State, Inputs}) ->
    {NewState, NewInputs} = exec([I], State, Inputs),
    {Rest, NewState, NewInputs}.

exec(Instructions, Inputs) ->
    exec(Instructions, #reg{}, Inputs).

exec([{inp, Reg}|Rest], State, [I|Input]) ->
    exec(Rest, set_reg(Reg, State, I), Input);
exec([{Op, Reg, Arg}|Rest], State, Input) ->
    Val1 = get_reg(Reg, State),
    Val2 = get_val(Arg, State),
    exec(Rest, set_reg(Reg, State, apply_op(Op, Val1, Val2)), Input);
exec([], State, Input) ->
    {State, Input}.

apply_op(Op, A, B) when is_integer(A), is_integer(B) ->
    case Op of
        add -> A + B;
        mul -> A * B;
        dib -> A div B;
        mod -> A rem B;
        eql ->
            case A == B of
                true -> 1;
                false -> 0
            end
    end;
%%% symbolic op implementations
apply_op(add, A, B) ->
    case {A, B} of
        {_, 0} -> A;
        {0, _} -> B;
        {_, _} when is_integer(A), is_integer(B) ->
            A + B;
        {_, _} when is_integer(B)->
            case value_range(A) of
                List=[H|_] when is_integer(H) ->
                    [X+B || X <- List];
                _ ->
                    {add, A, B}
            end;
        {_,_} when is_integer(A) ->
            case value_range(B) of
                List=[H|_] when is_integer(H) ->
                    [X+A || X <- List];
                _ ->
                    {add, A, B}
            end;
        {_, _} when is_list(A), is_list(B) ->
            [X+Y || X <- A, Y <- B];
        {_, _} ->
            {add, A, B}
    end;
apply_op(mul, A, B) ->
    case {A, B} of
        {_, 0} -> 0;
        {0, _} -> 0;
        {_, 1} -> A;
        {1, _} -> B;
        {_, _} when is_integer(A), is_integer(B) ->
            A * B;
        {_, _} when is_integer(B)->
            case value_range(A) of
                List=[H|_] when is_integer(H) ->
                    [X*B || X <- List];
                _ ->
                    {mul, A, B}
            end;
        {_,_} when is_integer(A) ->
            case value_range(B) of
                List=[H|_] when is_integer(H) ->
                    [X*A || X <- List];
                _ ->
                    {mul, A, B}
            end;
        {_, _} ->
            {mul, A, B}
    end;
apply_op(dib, A, B) ->
    case {A, B} of
        {_, 1} -> A;
        {_, _} when is_integer(A), is_integer(B) ->
            A div B;
        {_, _} when is_integer(B)->
            case value_range(A) of
                List=[H|_] when is_integer(H) ->
                    [X div B || X <- List];
                _ ->
                    {dib, A, B}
            end;
        {_,_} when is_integer(A) ->
            case value_range(B) of
                List=[H|_] when is_integer(H) ->
                    [X div A || X <- List];
                _ ->
                    {dib, A, B}
            end;
        {A, B} ->
            {dib, A, B}
    end;
apply_op(mod, A, B) ->
    case {A, B} of
        {_, 1} -> 0;
        {_, _} when is_integer(A), is_integer(B) ->
            A rem B;
        {_, _} when is_integer(B)->
            case value_range(A) of
                List=[H|_] when is_integer(H) ->
                    [X rem B || X <- List];
                _ ->
                    {mod, A, B}
            end;
        {_, _} when is_integer(A) ->
            case value_range(B) of
                List=[H|_] when is_integer(H) ->
                    [a rem X || X <- List];
                _ ->
                    {mod, A, B}
            end;
        {A, B} ->
            {mod, A, B}
    end;
apply_op(eql, A, B) when is_integer(A), is_integer(B) ->
    case A == B of
        true -> 1;
        false -> 0
    end;
apply_op(eql, A, B) ->
    case A == B of
        true -> 1;
        false ->
            AllA = value_range(A),
            AllB = value_range(B),
            case {lists:all(fun erlang:is_integer/1, AllA),
                  lists:all(fun erlang:is_integer/1, AllB),
                  ordsets:intersection(AllA, AllB)} of
                {true, true, []} ->
                    %% can never be equal
                    0;
                _ ->
                    {eql, A, B}
            end
    end.

value_range(A) when is_list(A) ->
    ordsets:from_list(lists:flatten([value_range(X) || X <- A]));
value_range(A) ->
    [A].

expr_depth({_Op, A, B}) ->
    1 + max(expr_depth(A), expr_depth(B));
expr_depth(_) ->
    1.

depends_on(Val, {_Op, A, B}) ->
    (A == Val) orelse (B == Val)
        orelse depends_on(Val, A) orelse depends_on(Val, B);
depends_on(_Val, _) ->
    false.

print({Op, A, B}) ->
    ["(", print(A), op_char(Op), print(B), ")"];
print(V) when is_atom(V) ->
    atom_to_list(V);
print(V) ->
    integer_to_list(V).

op_char(add) -> " + ";
op_char(mul) -> " * ";
op_char(dib) -> " / ";
op_char(mod) -> " % ";
op_char(eql) -> " ? ".
