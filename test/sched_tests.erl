-module(sched_tests).

-include_lib("eunit/include/eunit.hrl").

run_without_failure_test_() ->
    Cases = [
        {
            "2 msgs at concurrency 3",
            fun(V) ->
                N = rand:uniform(10),
                timer:sleep(N),
                V
            end,
            [1, 2],
            3
        },
        {
            "5 msgs at concurrency 5",
            fun(V) ->
                N = rand:uniform(10),
                timer:sleep(N),
                V
            end,
            [1, 2, 3, 4, 5],
            5
        },
        {
            "10 msgs at concurrency 4, with random delay on each message",
            fun(V) ->
                N = rand:uniform(10),
                timer:sleep(N),
                V
            end,
            [1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
            4
        }
    ],
    F = fun({Name, Func, Input, Max}) ->
        {Name, fun() ->
            Ret = sched:run(Func, Input, Max),
            Actual = lists:sort([V || {ok, V} <- Ret]),
            ?assertEqual(Input, Actual)
        end}
    end,
    lists:map(F, Cases).

run_with_failures_test_() ->
    Input = [2, 3, 0, 1],
    Func = fun(V) ->
        case V of
            2 -> exit("2 is BAD!!");
            3 -> throw("3 is AWEFUL!!");
            _ -> 10 / V
        end
    end,
    Ret = sched:run(Func, Input, 1),
    Success = [V || {ok, V} <- Ret],
    Errors = [E || {error, _} = E <- Ret],
    Exits = [E || {exit, _} = E <- Ret],
    Thrown = [E || {throw, _} = E <- Ret],
    [
        ?_assertEqual([10.0], Success),
        ?_assertMatch([{error, {badarith, _}}], Errors),
        ?_assertEqual([{exit, "2 is BAD!!"}], Exits),
        ?_assertEqual([{throw, "3 is AWEFUL!!"}], Thrown)
    ].
