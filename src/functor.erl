-module(functor).

-type type(_) :: any().
-callback map(fun((A) -> B)) -> fun((type(A)) -> type(B)).
-callback map(fun((A) -> B), type(A)) -> type(B).
