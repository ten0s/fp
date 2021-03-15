-module(result_tests).

-include_lib("eunit/include/eunit.hrl").
-include("fp.hrl").

ok_test() ->
    I0 = result:new("  64 "),
    I1 = result:map(fun string:strip/1, I0),
    I2 = result:map(fun erlang:list_to_integer/1, I1),
    I3 = result:map(fun (N) -> N + 1 end, I2),
    I4 = result:map(fun (N) -> [N] end, I3),
    I5 = result:chain(compose([fun result:new/1, fun string:to_lower/1]), I4),
    I6 = result:fold({fun fp:id/1, fun fp:id/1}, I5),
    ?assertEqual("a", I6).

error_test() ->
    I0 = result:new("  64 "),
    I1 = result:map(fun string:strip/1, I0),
    I2 = result:chain(fun (_) -> {error, error1} end, I1),
    I3 = result:map(fun (N) -> N + 1 end, I2),
    I4 = result:map(fun (N) -> [N] end, I3),
    I5 = result:map(fun string:to_lower/1, I4),
    I6 = result:fold({fun fp:id/1, fun fp:id/1}, I5),
    ?assertEqual(error1, I6).
