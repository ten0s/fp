-module(fp).

-export([
    id/1,
    compose/1,

    tap/1,
    tap/2,

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

id(X) -> X.

compose(Funs) ->
    fun (In) ->
        lists:foldr(fun (Fun, Acc) -> Fun(Acc) end, In, Funs)
    end.

tap(Fun) -> fun (X) ->
    Fun(X),
    X
end.

tap(Fun, X) ->
    (tap(Fun))(X).

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
