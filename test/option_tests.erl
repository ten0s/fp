-module(option_tests).

-include_lib("eunit/include/eunit.hrl").
-include("fp.hrl").

ok_test() ->
    I0 = option:new("  64 "),
    I1 = option:map(fun string:strip/1, I0),
    I2 = option:map(fun erlang:list_to_integer/1, I1),
    I3 = option:map(fun (N) -> N + 1 end, I2),
    I4 = option:map(fun (N) -> [N] end, I3),
    I5 = option:chain(compose([fun option:new/1, fun string:to_lower/1]), I4),
    I6 = option:fold({fun fp:id/1, fun fp:id/1}, I5),
    ?assertEqual("a", I6).

error_test() ->
    I0 = option:new("  64 "),
    I1 = option:map(fun string:strip/1, I0),
    I2 = option:chain(fun (_) -> none end, I1),
    I3 = option:map(fun (N) -> N + 1 end, I2),
    I4 = option:map(fun (N) -> [N] end, I3),
    I5 = option:map(fun string:to_lower/1, I4),
    I6 = option:fold({fun () -> error_handled end, fun fp:id/1}, I5),
    ?assertEqual(error_handled, I6).
