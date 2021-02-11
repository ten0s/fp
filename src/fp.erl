-module(fp).

-export([
    id/1,
    cons/2,

    compose/1,
    pipe/1,

    tap/1,
    tap/2,

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
    monad/1
]).

-type monad(T) :: identity:type(T)
                | error_m:type(T).

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

tap(Fun) -> fun (X) ->
    tap(Fun, X)
end.

tap(Fun, X) ->
    Fun(X),
    X.

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
    fun (Monad) ->
        fun (Module) ->
            Module:What(Fun, Monad)
        end
    end.

-spec map(fun((A) -> B)) -> fun((monad(A)) -> monad(B)).
map(Fun) ->
    call(?FUNCTION_NAME, Fun).
    
-spec map(fun((A) -> B), monad(A)) -> monad(B).
map(Fun, Mon) ->
    (map(Fun))(Mon).

-spec chain(fun((A) -> B)) -> fun((monad(A)) -> B).
chain(Fun) ->
    call(?FUNCTION_NAME, Fun).

-spec chain(fun((A) -> B), monad(A)) -> B.
chain(Fun, Mon) ->
    (chain(Fun))(Mon).

-spec fold(fun((A) -> B)) -> fun((monad(A)) -> B).
fold(Fun) ->
    call(?FUNCTION_NAME, Fun).

-spec fold(fun((A) -> B), monad(A)) -> B.
fold(Fun, Mon) ->
    (fold(Fun))(Mon).

%-spec do([fun((A) -> B | monad(B))], A) -> B.
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
