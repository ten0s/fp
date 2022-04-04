-module(fp_tests).

-include_lib("eunit/include/eunit.hrl").
-include("fp.hrl").

%% identity_law_test_() ->
%%     %% fmap id = id
%%     Map = erlz:curried(fun map/2),
%%     Func1 = {value, 1},
%%     Func2 = {list, [{value, 1}, {value, 2}]},
%%     [
%%         ?_assertEqual((Map(fun id/1))(Func1), id(Func1)),
%%         ?_assertEqual((Map(fun id/1))(Func2), id(Func2))
%%     ].

%% composition_law_test_() ->
%%     %% fmap (f . g)  ==  fmap f . fmap g
%%     Map = erlz:curried(fun map/2),
%%     F = fun (X) -> X + 1 end,
%%     G = fun (X) -> 2 * X end,
%%     Func1 = {value, 1},
%%     Func2 = {list, [{value, 1}, {value, 2}]},
%%     [
%%         ?_assertEqual((Map(compose(F, G)))(Func1), (compose(Map(F), Map(G)))(Func1)),
%%         ?_assertEqual((Map(compose(F, G)))(Func2), (compose(Map(F), Map(G)))(Func2))
%%     ].

id_test() ->
    %% TODO: forall x: x = id(x)
    ?assertEqual(1, fp:id(1)),
    ?assertEqual(one, fp:id(one)),
    ?assertEqual("one", fp:id("one")).

cons_test() ->
    Xs = [1, 2, 3, 4, 5],
    Ys = lists:foldl(fun fp:cons/2, [], Xs),
    Zs = lists:foldr(fun fp:cons/2, [], Xs),
    ?assertEqual(lists:reverse(Xs), Ys),
    ?assertEqual(Xs, Zs).

compose_test() ->
    %% TODO: what are compose props?
    %% Associativity: f ⋅ g ⋅ h = (f ⋅ g) ⋅ h = f ⋅ (g ⋅ h)
    Inc = fun (X) -> X + 1 end,
    Mul = fun (X) -> fun (Y) -> X * Y end end,
    ?assertEqual(1, (compose([]))(1)),
    ?assertEqual(1, (compose([fun fp:id/1]))(1)),
    ?assertEqual(1, (compose([fun fp:id/1, fun fp:id/1]))(1)),
    ?assertEqual(3, (compose([Inc, Mul(2)]))(1)),
    ?assertEqual(5, (compose([Inc, Mul(2), Inc]))(1)).

flip_test() ->
    ?assertEqual("B->A", (fp:flip(fun (A, B) -> A ++ "->" ++ B end))("A", "B")),
    ?assertEqual(2.0, (fp:flip(fun erlang:'/'/2))(1, 2)).

pipe_test() ->
    %% TODO: what are pipe props?
    Inc = fun (X) -> X + 1 end,
    Mul = fun (X) -> fun (Y) -> X * Y end end,
    ?assertEqual(1, (pipe([]))(1)),
    ?assertEqual(1, (pipe([fun fp:id/1]))(1)),
    ?assertEqual(1, (pipe([fun fp:id/1, fun fp:id/1]))(1)),
    ?assertEqual(4, (pipe([Inc, Mul(2)]))(1)),
    ?assertEqual(5, (pipe([Inc, Mul(2), Inc]))(1)).

do_identity_1_test() ->
    In = identity:new("  64 "),
    Out = fp:do(identity, [
        fun (I) -> fp:map(fun string:strip/1, I) end,
        fun (I) -> fp:map(fun erlang:list_to_integer/1, I) end,
        fun (I) -> fp:map(fun (N) -> N + 1 end, I) end,
        fun (I) -> fp:map(fun (N) -> [N] end, I) end,
        fun (I) -> fp:chain(compose([fun identity:new/1, fun string:to_lower/1]), I) end,
        fun (I) -> fp:fold(fun fp:id/1, I) end
    ], In),
    ?assertEqual("a", Out).

do_identity_2_test() ->
    In = identity:new("  64 "),
    Out = fp:do(identity, [
        fp:map(fun string:strip/1),
        fp:map(fun erlang:list_to_integer/1),
        fp:map(fun (N) -> N + 1 end),
        fp:map(fun (N) -> [N] end),
        fp:chain(compose([fun identity:new/1, fun string:to_lower/1])),
        fp:fold(fun fp:id/1)
    ], In),
    ?assertEqual("a", Out).

do_identity_3_test() ->
    In = identity:new("  64 "),
    Out = fp:do(identity, [
        map(fun string:strip/1),
        map(fun erlang:list_to_integer/1),
        map(fun (N) -> N + 1 end),
        map(fun (N) -> [N] end),
        chain(compose([fun identity:new/1, fun string:to_lower/1])),
        fold(fun fp:id/1)
    ], In),
    ?assertEqual("a", Out).

do_identity_4_test() ->
    In = identity:new("  64 "),
    Out = fp:do(identity, [
        {map, fun string:strip/1},
        {map, fun erlang:list_to_integer/1},
        {map, fun (N) -> N + 1 end},
        {map, fun (N) -> [N] end},
        {chain, compose([fun identity:new/1, fun string:to_lower/1])},
        {fold, fun fp:id/1}
    ], In),
    ?assertEqual("a", Out).

do_result_1_test() ->
    In = result:new("  64 "),
    Out = fp:do(result, [
        map(fun string:strip/1),
        map(fun erlang:list_to_integer/1),
        map(fun (N) -> N + 1 end),
        map(fun (N) -> [N] end),
        chain(compose([fun result:new/1, fun string:to_lower/1])),
        fold({fun fp:id/1, fun fp:id/1})
    ], In),
    ?assertEqual("a", Out).

do_maybe_1_test() ->
    In = maybe:new("  64 "),
    Out = fp:do(maybe, [
        map(fun string:strip/1),
        map(fun erlang:list_to_integer/1),
        map(fun (N) -> N + 1 end),
        map(fun (N) -> [N] end),
        chain(compose([fun maybe:new/1, fun string:to_lower/1])),
        fold({fun () -> error end, fun fp:id/1})
    ], In),
    ?assertEqual("a", Out).
