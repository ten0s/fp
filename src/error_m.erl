-module(error_m).

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

-opaque type(T) :: {ok, T} | {error, any()}.

-spec new(T) -> type(T).
new(Val) ->
    {ok, Val}.

-spec map(fun((A) -> B)) -> fun((type(A)) -> type(B)).
map(Fun) -> fun
    ({ok, Val}) -> {ok, Fun(Val)};
    ({error, _} = Error) -> Error
end.

-spec map(fun((A) -> B), type(A)) -> type(B).
map(Fun, Monad) ->
    (map(Fun))(Monad).

-spec chain(fun((A) -> B)) -> fun((type(A)) -> B).
chain(Fun) -> fun 
    ({ok, Val}) -> Fun(Val);
    ({error, _} = Error) -> Error
end.

-spec chain(fun((A) -> B), type(A)) -> B.
chain(Fun, Monad) ->
    (chain(Fun))(Monad).

%-spec fold(fun((A) -> B)) -> fun((type(A)) -> B).
fold({ErrFun, OkFun}) -> fun
    ({ok, Val}) -> OkFun(Val);
    ({error, Err}) -> ErrFun(Err)
end.

%-spec fold(fun((A) -> B), type(A)) -> B.
fold({ErrFun, OkFun}, Monad) ->
    (fold({ErrFun, OkFun}))(Monad).
