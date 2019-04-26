sched
=====

A scheduler library to spawn multiple worker processes with maximum concurrency.

Build
-----

    $ rebar3 compile

How to Use
----------

    Result = sched:run(
        fun(From, V) ->
          % do a slow task on given V, such as:
          timer:sleep(rand:uniform(100)),
          From ! V + 1
        end,
        [1, 2, 3, 4, 5],
        3
    )

and the result will be:

    [2, 3, 4, 5, 6]
