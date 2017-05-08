-module(expr).
-export([eval/2, print/1]).

-type expr() :: {num, integer()}
              | {var, atom()}
              | {add, expr(), expr()}
              | {mul, expr(), expr()}.
-type env() :: [{atom(), integer()}].

-type instr() :: {push, integer()}
               | {fetch, atom()}
               | {add2}
               | {mult2}.
-type program() :: [instr()].
-type stack() :: [integer()].

-spec lookup(atom(), env()) -> integer().

lookup(A, [{A, Val} | _]) ->
    Val;
lookup(A, [_|Rest]) ->
    lookup(A, Rest).

-spec print(expr()) -> string().

print({num, N}) ->
    integer_to_list(N);
print({var, A}) ->
    atom_to_list(A);
print({add, E1, E2}) ->
    "(" ++ print(E1) ++ " + " ++ print(E2) ++ ")";
print({mul, E1, E2}) ->
    "(" ++ print(E1) ++ " * " ++ print(E2) ++ ")".

-spec eval(env(), expr()) -> integer().

eval(_Env, {num, N}) ->
    N;
eval(Env, {var, A}) ->
    lookup(A, Env);
eval(Env, {add, E1, E2}) ->
    eval(Env, E1) + eval(Env, E2);
eval(Env, {mul, E1, E2}) ->
    eval(Env, E1) * eval(Env, E2).

-spec simplify_mul0(expr()) -> expr().

simplify_mul0({mul, {num, 0}, _E2}) ->
    {num, 0};
simplify_mul0({mul, _E1, {num, 0}}) ->
    {num, 0};
simplify_mul0(X) ->
    X.

-spec simplify_mul1(expr()) -> expr().

simplify_mul1({mul, {num, 1}, E2}) ->
    E2;
simplify_mul1({mul, E1, {num, 1}}) ->
    E1;
simplify_mul1(X) -> X.

-spec simplify_add(expr()) -> expr().

simplify_add({add, {num, 0}, E2}) ->
    E2;
simplify_add({add, E1, {num, 0}}) ->
    E1;
simplify_add(X) -> X.

-spec compose([fun((T) -> T)]) -> fun((T) -> T).

compose([F | []]) ->
    F;
compose([Rule | Rules]) ->
    fun (E) -> (compose(Rules))(Rule(E)) end.

-spec rules() -> [fun((expr()) -> expr())].

rules() ->
    [fun simplify_mul0/1, fun simplify_mul1/1, fun simplify_add/1].

-spec simp(fun((expr()) -> expr()), expr()) -> expr().

simp(Simplifiers, {add, E1, E2}) ->
    Simplifiers({add, simp(Simplifiers, E1), simp(Simplifiers, E2)});
simp(Simplifiers, {mul, E1, E2}) ->
    Simplifiers({mul, simp(Simplifiers, E1), simp(Simplifiers, E2)});
simp(_, E) -> E.

-spec simplify(expr()) -> expr().

simplify(E) ->
    simp(compose(rules()), E).

-spec compile(expr()) -> program().

compile({num, N}) ->
    [{push, N}];
compile({var, A}) ->
    [{fetch, A}];
compile({add, E1, E2}) ->
    compile(E1) ++ compile(E2) ++ [{add2}];
compile({mul, E1, E2}) ->
    compile(E1) ++ compile(E2) ++ [{mul2}].

-spec run(program(), env(), stack()) -> integer().

run([{push, N} | Rest], Env, Stack) ->
    run(Rest, Env, [N | Stack]);
run([{fetch, A} | Rest], Env, Stack) ->
    run(Rest, Env, [lookup(A, Env) | Stack]);
run([{add2} | Rest], Env, [X , Y | RestStack]) ->
    run(Rest, Env, [X + Y | RestStack]) ;
run([{mul2} | Rest], Env, [X , Y | RestStack]) ->
    run(Rest, Env, [X * Y | RestStack]) ;
run([], _Env, [N]) -> N.

-spec run(program(), env()) -> integer().

run(Prog, Env) ->
    run(Prog, Env, []).

-spec get_while(fun((T) -> boolean()), [T]) -> {[T], [T]}.

get_while(Predicate, [X | Rest]) ->
    case Predicate(X) of
        false ->
            {[], [X | Rest]};
        true ->
            {TrueSlice, FalseSlice} = get_while(Predicate, Rest),
            {[X | TrueSlice], FalseSlice}
    end;
get_while(_Predicate, []) ->
    {[], []}.

-spec is_alpha(integer()) -> boolean().

is_alpha(Ch) -> $a =< Ch andalso Ch =< $z.

-spec is_num(integer()) -> boolean().

is_num(Ch) -> $0 =< Ch andalso Ch =< $9.

-spec parse(string()) -> {expr(), string()}.

parse([$( | Rest]) ->
    {E1, Rest1} = parse(Rest),
    [Op | Rest2] = Rest1,
    {E2, Rest3} = parse(Rest2),
    [$), RestFinal] = Rest3,
    {
      case Op of
          $+ -> {add, E1, E2};
          $* -> {mul, E1, E2}
      end,
      RestFinal};
parse([Ch | Rest]) when $a =< Ch andalso Ch =< $z ->
    {VarName, Remainder} = get_while(fun is_alpha/1, Rest),
    {{var, list_to_atom([Ch | VarName])}, Remainder};
parse([Ch | Rest]) when $0 =< Ch andalso Ch =< $9 ->
    {NumStr, Remainder} = get_while(fun is_num/1, Rest),
    {{num, list_to_integer([Ch | NumStr])}, Remainder}.

-spec exp1() -> expr().

exp1() ->
    {add, {num, 1}, {mul, {num, 3}, {num, 0}}}.
