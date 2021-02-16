-module(maybe_m_tests).

-include_lib("eunit/include/eunit.hrl").
-include("fp.hrl").

ok_test() ->
    I0 = maybe_m:new("  64 "),
    I1 = maybe_m:map(fun string:strip/1, I0),
    I2 = maybe_m:map(fun erlang:list_to_integer/1, I1),
    I3 = maybe_m:map(fun (N) -> N + 1 end, I2),
    I4 = maybe_m:map(fun (N) -> [N] end, I3),
    I5 = maybe_m:chain(compose([fun maybe_m:new/1, fun string:to_lower/1]), I4),
    I6 = maybe_m:fold({fun fp:id/1, fun fp:id/1}, I5),
    ?assertEqual("a", I6).

error_test() ->
    I0 = maybe_m:new("  64 "),
    I1 = maybe_m:map(fun string:strip/1, I0),
    I2 = maybe_m:chain(fun (_) -> nothing end, I1),
    I3 = maybe_m:map(fun (N) -> N + 1 end, I2),
    I4 = maybe_m:map(fun (N) -> [N] end, I3),
    I5 = maybe_m:map(fun string:to_lower/1, I4),
    I6 = maybe_m:fold({fun () -> error_handled end, fun fp:id/1}, I5),
    ?assertEqual(error_handled, I6).
