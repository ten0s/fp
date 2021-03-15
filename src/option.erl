-module(option).

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
    type/1
]).

-opaque type(T) :: {some, T} | none.

-spec new(T) -> type(T).
new(Val) ->
    {some, Val}.

-spec map(fun((A) -> B)) -> fun((type(A)) -> type(B)).
map(Fun) -> fun
    ({some, Val}) -> {some, Fun(Val)};
    (none)        -> none
end.

-spec map(fun((A) -> B), type(A)) -> type(B).
map(Fun, Option) ->
    (map(Fun))(Option).

-spec chain(fun((A) -> type(B))) -> fun((type(A)) -> type(B)).
chain(Fun) -> fun 
    ({some, Val}) -> Fun(Val);
    (none)        -> none
end.

-spec chain(fun((A) -> type(B)), type(A)) -> type(B).
chain(Fun, Option) ->
    (chain(Fun))(Option).

-spec fold({fun(() -> B), fun((A) -> B)}) -> fun((type(A)) -> B).
fold({NoneFun, SomeFun}) -> fun
    ({some, Val}) -> SomeFun(Val);
    (none)        -> NoneFun()
end.

-spec fold({fun(() -> B), fun((A) -> B)}, type(A)) -> B.
fold({NoneFun, SomeFun}, Option) ->
    (fold({NoneFun, SomeFun}))(Option).
