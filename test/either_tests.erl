-module(either_tests).

-include_lib("eunit/include/eunit.hrl").
-include("fp.hrl").

ok_test() ->
    I0 = either:new("  64 "),
    I1 = either:map(fun string:strip/1, I0),
    I2 = either:map(fun erlang:list_to_integer/1, I1),
    I3 = either:map(fun (N) -> N + 1 end, I2),
    I4 = either:map(fun (N) -> [N] end, I3),
    I5 = either:chain(compose([fun either:new/1, fun string:to_lower/1]), I4),
    I6 = either:fold({fun fp:id/1, fun fp:id/1}, I5),
    ?assertEqual("a", I6).

error_test() ->
    I0 = either:new("  64 "),
    I1 = either:map(fun string:strip/1, I0),
    I2 = either:chain(fun (_) -> {left, error} end, I1),
    I3 = either:map(fun (N) -> N + 1 end, I2),
    I4 = either:map(fun (N) -> [N] end, I3),
    I5 = either:map(fun string:to_lower/1, I4),
    I6 = either:fold({fun erlang:atom_to_list/1, fun fp:id/1}, I5),
    ?assertEqual("error", I6).
