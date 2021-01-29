-module(identity_m).

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

-opaque type(T) :: T.

-spec new(T) -> type(T).
new(Val) ->
    Val.

-spec map(fun((A) -> B)) -> fun((type(A)) -> type(B)).
map(Fun) ->
    Fun.

-spec map(fun((A) -> B), type(A)) -> type(B).
map(Fun, Id) ->
    (map(Fun))(Id).

-spec chain(fun((A) -> B)) -> fun((type(A)) -> B).
chain(Fun) ->
    Fun.

-spec chain(fun((A) -> B), type(A)) -> B.
chain(Fun, Id) ->
    (chain(Fun))(Id).

-spec fold(fun((A) -> B)) -> fun((type(A)) -> B).
fold(Fun) ->
    Fun.

-spec fold(fun((A) -> B), type(A)) -> B.
fold(Fun, Id) ->
    (fold(Fun))(Id).
