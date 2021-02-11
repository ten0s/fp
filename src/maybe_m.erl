-module(maybe_m).

-export([
    new/1,
    map/1,
    map/2,
    chain/1,
    chain/2,
    fold/1,
    fold/2
]).

-export_type([
    type/1
]).

-opaque type(T) :: {ok, T}    | error
                 | {value, T} | false.

-spec new(T) -> type(T).
new(Val) ->
    {ok, Val}.

-spec map(fun((A) -> B)) -> fun((type(A)) -> type(B)).
map(Fun) -> fun
    ({ok, Val})    -> {ok, Fun(Val)};
    ({value, Val}) -> {ok, Fun(Val)};
    (error)        -> error;
    (false)        -> false
end.

-spec map(fun((A) -> B), type(A)) -> type(B).
map(Fun, Monad) ->
    (map(Fun))(Monad).

-spec chain(fun((A) -> type(B))) -> fun((type(A)) -> type(B)).
chain(Fun) -> fun 
    ({ok, Val})    -> Fun(Val);
    ({value, Val}) -> Fun(Val);
    (error)        -> error;
    (false)        -> false
end.

-spec chain(fun((A) -> type(B)), type(A)) -> type(B).
chain(Fun, Monad) ->
    (chain(Fun))(Monad).

-spec fold(fun((A) -> B)) -> fun((type(A)) -> B).
fold({ErrFun, OkFun}) -> fun
    ({ok, Val})    -> OkFun(Val);
    ({value, Val}) -> OkFun(Val);
    (error)        -> ErrFun();
    (false)        -> ErrFun()
end.

-spec fold(fun((A) -> B), type(A)) -> B.
fold({ErrFun, OkFun}, Monad) ->
    (fold({ErrFun, OkFun}))(Monad).
