-module(sched_tests).

-include_lib("eunit/include/eunit.hrl").

run_test_() ->
    Cases = [
        {
            "2 msgs at concurrency 3",
            fun(From, V) ->
                N = rand:uniform(10),
                timer:sleep(N),
                From ! V
            end,
            [1, 2],
            3
        },
        {
            "5 msgs at concurrency 5",
            fun(From, V) ->
                N = rand:uniform(10),
                timer:sleep(N),
                From ! V
            end,
            [1, 2, 3, 4, 5],
            5
        },
        {
            "10 msgs at concurrency 4, with random delay on each message",
            fun(From, V) ->
                N = rand:uniform(10),
                timer:sleep(N),
                From ! V
            end,
            [1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
            4
        }
    ],
    F = fun({Name, Func, Input, Max}) ->
        Actual = sched:run(Func, Input, Max),
        {Name, [?_assertEqual(Input, lists:sort(Actual))]}
    end,
    lists:map(F, Cases).
