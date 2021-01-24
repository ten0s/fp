-module(fp_tests).

-include_lib("eunit/include/eunit.hrl").

do_1_test() ->
    I0 = identity:new("  64 "),
    I1 = fp:map(fun string:strip/1, I0),
    I2 = fp:map(fun erlang:list_to_integer/1, I1),
    I3 = fp:map(fun (N) -> N + 1 end, I2),
    I4 = fp:map(fun (N) -> [N] end, I3),
    I5 = fp:map(fun string:to_lower/1, I4),
    I6 = fp:fold(fun fp:id/1, I5),
    ?assertEqual("a", I6).

do_2_test() ->
    In = identity:new("  64 "),
    Out = fp:do([
        fun (I) -> fp:map(fun string:strip/1, I) end,
        fun (I) -> fp:map(fun erlang:list_to_integer/1, I) end,
        fun (I) -> fp:map(fun (N) -> N + 1 end, I) end,
        fun (I) -> fp:map(fun (N) -> [N] end, I) end,
        fun (I) -> fp:map(fun string:to_lower/1, I) end,
        fun (I) -> fp:fold(fun fp:id/1, I) end
    ], In),
    ?assertEqual("a", Out).

do_3_test() ->
    In = identity:new("  64 "),
    Out = fp:do([
        fp:map(fun string:strip/1),
        fp:map(fun erlang:list_to_integer/1),
        fp:map(fun (N) -> N + 1 end),
        fp:map(fun (N) -> [N] end),
        fp:map(fun string:to_lower/1),
        fp:fold(fun fp:id/1)
    ], In),
    ?assertEqual("a", Out).

do_4_test() ->
    In = identity:new("  64 "),
    Out = fp:do([
        fp:map(fun string:strip/1),
        fp:map(fun erlang:list_to_integer/1),
        fp:map(fun (N) -> N + 1 end),
        fp:map(fun (N) -> [N] end),
        fp:map(fun string:to_lower/1),
        fp:fold(fun fp:id/1)
    ], In),
    ?assertEqual("a", Out).

do_5_test() ->
    In = identity:new("  64 "),
    Out = fp:do([
        {map, fun string:strip/1},
        {map, fun erlang:list_to_integer/1},
        {map, fun (N) -> N + 1 end},
        {map, fun (N) -> [N] end},
        {map, fun string:to_lower/1},
        {fold, fun fp:id/1}
    ], In),
    ?assertEqual("a", Out).
