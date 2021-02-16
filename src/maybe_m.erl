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

%% TBD: It's pretty much acceptable to also use
%% {ok, T} | error and {value, T} | false here
%% as long as the monad is unchanged

-opaque type(T) :: {just, T}  | nothing.

-spec new(T) -> type(T).
new(Val) ->
    {just, Val}.

-spec map(fun((A) -> B)) -> fun((type(A)) -> type(B)).
map(Fun) -> fun
    ({just, Val}) -> {just, Fun(Val)};
    (nothing)     -> nothing
end.

-spec map(fun((A) -> B), type(A)) -> type(B).
map(Fun, Monad) ->
    (map(Fun))(Monad).

-spec chain(fun((A) -> type(B))) -> fun((type(A)) -> type(B)).
chain(Fun) -> fun 
    ({just, Val})  -> Fun(Val);
    (nothing)      -> nothing
end.

-spec chain(fun((A) -> type(B)), type(A)) -> type(B).
chain(Fun, Monad) ->
    (chain(Fun))(Monad).

-spec fold(fun((A) -> B)) -> fun((type(A)) -> B).
fold({NothingFun, JustFun}) -> fun
    ({just, Val}) -> JustFun(Val);
    (nothing)     -> NothingFun()
end.

-spec fold(fun((A) -> B), type(A)) -> B.
fold({NothingFun, JustFun}, Monad) ->
    (fold({NothingFun, JustFun}))(Monad).
