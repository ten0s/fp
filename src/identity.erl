-module(identity).

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
    identity/1
]).

-record(identity, {
    val :: any()
}).

-opaque identity(_T) :: #identity{}.

-include_lib("eunit/include/eunit.hrl").

-spec new(T) -> identity(T).
new(Val) ->
    #identity{val=Val}.

-spec map(fun((A) -> B)) -> fun((identity(A)) -> identity(B)).
map(Fun) -> fun
    (#identity{val=Val}) -> #identity{val=Fun(Val)}
end.

-spec map(fun((A) -> B), identity(A)) -> identity(B).
map(Fun, Id) ->
    (map(Fun))(Id).

-spec chain(fun((A) -> B)) -> fun((identity(A)) -> B).
chain(Fun) -> fun 
    (#identity{val=Val}) -> Fun(Val)
end.

-spec chain(fun((A) -> B), identity(A)) -> B.
chain(Fun, Id) ->
    (chain(Fun))(Id).

-spec fold(fun((A) -> B)) -> fun((identity(A)) -> B).
fold(Fun) -> fun
    (#identity{val=Val}) -> Fun(Val)
end.

-spec fold(fun((A) -> B), identity(A)) -> B.
fold(Fun, Id) ->
    (fold(Fun))(Id).
