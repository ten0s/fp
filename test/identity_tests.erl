-module(identity_tests).

-include_lib("eunit/include/eunit.hrl").
-include("fp.hrl").

ok_test() ->
    I0 = identity:new("  64 "),
    I1 = identity:map(fun string:strip/1, I0),
    I2 = identity:map(fun erlang:list_to_integer/1, I1),
    I3 = identity:map(fun (N) -> N + 1 end, I2),
    I4 = identity:map(fun (N) -> [N] end, I3),
    I5 = identity:chain(compose([fun identity:new/1, fun string:to_lower/1]), I4),
    I6 = identity:fold(fun fp:id/1, I5),
    ?assertEqual("a", I6).
