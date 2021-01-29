-module(identity_m_tests).

-include_lib("eunit/include/eunit.hrl").
-include("fp.hrl").

ok_test() ->
    I0 = identity_m:new("  64 "),
    I1 = identity_m:map(fun string:strip/1, I0),
    I2 = identity_m:map(fun erlang:list_to_integer/1, I1),
    I3 = identity_m:map(fun (N) -> N + 1 end, I2),
    I4 = identity_m:map(fun (N) -> [N] end, I3),
    I5 = identity_m:chain(fun string:to_lower/1, I4),
    I6 = identity_m:fold(fun fp:id/1, I5),
    ?assertEqual("a", I6).
