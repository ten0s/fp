-module(either).

-behaviour(functor).

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
    type/2
]).

-opaque type(A, B) :: {left, A} | {right, B}.

-spec new(B) -> type(_A, B).
new(Val) ->
    {right, Val}.

-spec map(fun((B) -> C)) -> fun((type(A, B)) -> type(A, C)).
map(Fun) -> fun
    ({left, L})  -> {left, L};
    ({right, R}) -> {right, Fun(R)}
end.

-spec map(fun((B) -> C), type(A, B)) -> type(A, C).
map(Fun, Either) ->
    (map(Fun))(Either).

-spec chain(fun((B) -> type(A, C))) -> fun((type(A, B)) -> type(A, C)).
chain(Fun) -> fun 
    ({left, L})  -> {left, L};
    ({right, R}) -> Fun(R)
end.

-spec chain(fun((B) -> type(A, C)), type(A, B)) -> type(A, C).
chain(Fun, Either) ->
    (chain(Fun))(Either).

-spec fold({fun((A) -> C), fun((B) -> C)}) -> fun((type(A, B)) -> C).
fold({LeftFun, RightFun}) -> fun
    ({left, L})  -> LeftFun(L);
    ({right, R}) -> RightFun(R)
end.

-spec fold({fun((A) -> C), fun((B) -> C)}, type(A, B)) -> C.
fold({LeftFun, RightFun}, Either) ->
    (fold({LeftFun, RightFun}))(Either).
