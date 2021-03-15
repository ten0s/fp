-module(fp).

-export([
    id/1,
    cons/2,

    compose/1,
    pipe/1,

    tap/1,
    tap/2,
    flip/1,

    if_else/1,
    if_else/2,
    if_else/3,
    if_else/4,

    if_then/1,
    if_then/2,
    if_then/3,

    if_not/1,
    if_not/2,
    if_not/3,

    map/1,
    map/2,

    chain/1,
    chain/2,

    fold/1,
    fold/2,

    do/2,
    do/3
]).

-export_type([
    type/1
]).

-type type(_T) :: any().

-spec id(A) -> A.
id(X) -> X.

-spec cons(A, [A]) -> [A].
cons(X, Xs) ->
    [X | Xs].

compose(Funs) ->
    fun (In) ->
        lists:foldr(fun (Fun, Acc) -> Fun(Acc) end, In, Funs)
    end.

pipe(Funs) ->
    fun (In) ->
        lists:foldl(fun (Fun, Acc) -> Fun(Acc) end, In, Funs)
    end.

-spec tap(fun((A) -> _)) -> fun((A) -> A).
tap(Fun) -> fun (X) ->
    tap(Fun, X)
end.

-spec tap(fun((A) -> _), A) -> A.
tap(Fun, X) ->
    Fun(X),
    X.

-spec flip(fun((A, B) -> C)) -> fun((B, A) -> C).
flip(Fun) -> fun (X, Y) ->
    Fun(Y, X)
end.

if_else(Pred) -> fun (Then) -> fun (Else) -> fun (X) ->
    case Pred(X) of
    true ->
        Then(X);
    false ->
        Else(X)
    end
end end end.

if_else(Pred, Then) ->
    (if_else(Pred))(Then).

if_else(Pred, Then, Else) ->
    (if_else(Pred, Then))(Else).

if_else(Pred, Then, Else, X) ->
    (if_else(Pred, Then, Else))(X).

if_then(Pred) -> fun (Then) -> fun (X) ->
    case Pred(X) of
    true ->
        Then(X);
    false ->
        X
    end
end end.

if_then(Pred, Then) ->
    (if_then(Pred))(Then).

if_then(Pred, Then, X) ->
    (if_then(Pred, Then))(X).

if_not(Pred) -> fun (Fun) -> fun (X) ->
    case not Pred(X) of
    true ->
        Fun(X);
    false ->
        X
    end
end end.

if_not(Pred, Fun) ->
    (if_not(Pred))(Fun).

if_not(Pred, Fun, X) ->
    (if_not(Pred, Fun))(X).

call(What, Fun) ->
    fun (Type) ->
        fun (Module) ->
            Module:What(Fun, Type)
        end
    end.

-spec map(fun((A) -> B)) -> fun((type(A)) -> type(B)).
map(Fun) ->
    call(?FUNCTION_NAME, Fun).
    
-spec map(fun((A) -> B), type(A)) -> type(B).
map(Fun, Mon) ->
    (map(Fun))(Mon).

-spec chain(fun((A) -> B)) -> fun((type(A)) -> B).
chain(Fun) ->
    call(?FUNCTION_NAME, Fun).

-spec chain(fun((A) -> B), type(A)) -> B.
chain(Fun, Mon) ->
    (chain(Fun))(Mon).

%-spec fold(fun((A) -> B)) -> fun((type(A)) -> B).
fold(Fun) ->
    call(?FUNCTION_NAME, Fun).

%-spec fold(fun((A) -> B), type(A)) -> B.
fold(Fun, Mon) ->
    (fold(Fun))(Mon).

%-spec do([fun((A) -> B | type(B))], A) -> B.
do(Module, Funs) ->
    fun (In) ->
        lists:foldl(fun
            ({What, Fun}, Acc) ->
                Module:What(Fun, Acc);
            (Fun, Acc) ->
                (Fun(Acc))(Module)
        end, In, Funs)
    end.

do(Module, Funs, In) ->
    (do(Module, Funs))(In).
