[![Build Status](https://travis-ci.org/yowcow/sched.svg?branch=master)](https://travis-ci.org/yowcow/sched)

sched
=====

A scheduler library to spawn multiple worker processes with maximum concurrency.

Build
-----

    $ rebar3 compile

How to Use
----------

    Result = sched:run(
        fun(V) ->
          % do a slow task on given V, such as:
          timer:sleep(rand:uniform(100)),
          V + 1
        end,
        [1, 2, 3, 4, 5],
        3
    )

and the result will be:

    [
        {ok, 2},
        {ok, 3},
        {ok, 4},
        {ok, 5},
        {ok, 6}
    ]
