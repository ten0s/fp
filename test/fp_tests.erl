-module(fp_tests).

-include_lib("eunit/include/eunit.hrl").
-include("fp.hrl").

id_test() ->
    %% TODO: forall x: x = id(x)
    ?assertEqual(1, fp:id(1)),
    ?assertEqual(one, fp:id(one)),
    ?assertEqual("one", fp:id("one")).

compose_test() ->
    %% TODO: what are compose props?
    Inc = fun (X) -> X + 1 end,
    Mul = fun (X) -> fun (Y) -> X * Y end end,
    ?assertEqual(1, (compose([]))(1)),
    ?assertEqual(1, (compose([fun fp:id/1]))(1)),
    ?assertEqual(1, (compose([fun fp:id/1, fun fp:id/1]))(1)),
    ?assertEqual(3, (compose([Inc, Mul(2)]))(1)),
    ?assertEqual(5, (compose([Inc, Mul(2), Inc]))(1)).

do_identity_m_1_test() ->
    In = identity_m:new("  64 "),
    Out = fp:do(identity_m, [
        fun (I) -> fp:map(fun string:strip/1, I) end,
        fun (I) -> fp:map(fun erlang:list_to_integer/1, I) end,
        fun (I) -> fp:map(fun (N) -> N + 1 end, I) end,
        fun (I) -> fp:map(fun (N) -> [N] end, I) end,
        fun (I) -> fp:chain(fun string:to_lower/1, I) end,
        fun (I) -> fp:fold(fun fp:id/1, I) end
    ], In),
    ?assertEqual("a", Out).

do_identity_m_2_test() ->
    In = identity_m:new("  64 "),
    Out = fp:do(identity_m, [
        fp:map(fun string:strip/1),
        fp:map(fun erlang:list_to_integer/1),
        fp:map(fun (N) -> N + 1 end),
        fp:map(fun (N) -> [N] end),
        fp:chain(fun string:to_lower/1),
        fp:fold(fun fp:id/1)
    ], In),
    ?assertEqual("a", Out).

do_identity_3_test() ->
    In = identity_m:new("  64 "),
    Out = fp:do(identity_m, [
        map(fun string:strip/1),
        map(fun erlang:list_to_integer/1),
        map(fun (N) -> N + 1 end),
        map(fun (N) -> [N] end),
        chain(fun string:to_lower/1),
        fold(fun fp:id/1)
    ], In),
    ?assertEqual("a", Out).

do_identity_4_test() ->
    In = identity_m:new("  64 "),
    Out = fp:do(identity_m, [
        {map, fun string:strip/1},
        {map, fun erlang:list_to_integer/1},
        {map, fun (N) -> N + 1 end},
        {map, fun (N) -> [N] end},
        {chain, fun string:to_lower/1},
        {fold, fun fp:id/1}
    ], In),
    ?assertEqual("a", Out).

do_error_m_3_test() ->
    In = error_m:new("  64 "),
    Out = fp:do(error_m, [
        map(fun string:strip/1),
        map(fun erlang:list_to_integer/1),
        map(fun (N) -> N + 1 end),
        map(fun (N) -> [N] end),
        map(fun string:to_lower/1),
        fold({fun fp:id/1, fun fp:id/1})
    ], In),
    ?assertEqual("a", Out).
