-module(identity).

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

-opaque type(T) :: {T}.

-spec new(T) -> type(T).
new(Val) ->
    {Val}.

-spec map(fun((A) -> B)) -> fun((type(A)) -> type(B)).
map(Fun) -> fun (Mon) ->
    map(Fun, Mon)
end.

-spec map(fun((A) -> B), type(A)) -> type(B).
map(Fun, {Val}) ->
    {Fun(Val)}.

-spec chain(fun((A) -> type(B))) -> fun((type(A)) -> type(B)).
chain(Fun) -> fun (Mon) ->
    chain(Fun, Mon)
end.

-spec chain(fun((A) -> type(B)), type(A)) -> type(B).
chain(Fun, {Val}) ->
    Fun(Val).

-spec fold(fun((A) -> B)) -> fun((type(A)) -> B).
fold(Fun) -> fun (Mon) ->
    fold(Fun, Mon)
end.

-spec fold(fun((A) -> B), type(A)) -> B.
fold(Fun, {Val}) ->
    Fun(Val).
