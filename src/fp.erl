-module(fp).

-export([
    id/1,

    tap/1,
    tap/2,

    map/1,
    map/2,
    chain/1,
    chain/2,
    fold/1,
    fold/2,

    do/1,
    do/2
]).

-export_type([
    monad/1
]).

-type monad(T) :: identity:identity(T).

id(X) -> X.

tap(Fun) -> fun (X) ->
    Fun(X),
    X
end.

tap(Fun, X) ->
    (tap(Fun))(X).

call(What, Fun) ->
    fun (Mon) ->
        Mod = element(1, Mon),
        Mod:What(Fun, Mon)
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
chain(Fun, Id) ->
    (chain(Fun))(Id).

-spec fold(fun((A) -> B)) -> fun((monad(A)) -> B).
fold(Fun) ->
    call(?FUNCTION_NAME, Fun).

-spec fold(fun((A) -> B), monad(A)) -> B.
fold(Fun, Id) ->
    (fold(Fun))(Id).

%-spec do([fun((A) -> B | monad(B))], A) -> B.
do(Funs) ->
    fun (I) ->
        lists:foldl(fun
            ({What, Fun}, Acc) ->
                Module = element(1, Acc),
                Module:What(Fun, Acc);
            (Fun, Acc) ->
                Fun(Acc)
        end, I, Funs)
    end.

do(Funs, I) ->
    (do(Funs))(I).
