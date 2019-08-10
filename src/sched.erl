-module(sched).

-export([
    run/3
]).
-export_type([
    callback/0,
    jobs/0,
    result/0
]).

-type callback() :: fun((pid(), term()) -> ok).
-type jobs() :: [term()].
-type result() :: [term()].
-type state() :: #{
    f := callback(),
    jobs := jobs(),
    max := integer(),
    cur := integer()
}.

-spec run(callback(), jobs(), integer()) -> result().
run(F, Jobs, Max) ->
    run(#{
        cb => F,
        jobs => Jobs,
        max => Max,
        cur => 0
    }, []).

-spec run(state(), result()) -> result().
% when no more job to do, not working on anything
run(#{jobs := [], cur := 0}, Acc) -> Acc;
% when no more job to do, still working on jobs
run(#{jobs := [], cur := Cur} = State, Acc) ->
    Ret = wait(),
    run(State#{cur => Cur-1}, [Ret | Acc]);
% when having jobs to do, working at max concurrency
run(#{cb := F, jobs := [Job|Jobs], max := Max, cur := Cur} = State, Acc) when Cur >= Max ->
    Ret = wait(),
    work(F, Job),
    run(State#{jobs => Jobs}, [Ret|Acc]);
% when having jobs to do, still having free cap
run(#{cb := F, jobs := [Job|Jobs], cur := Cur} = State, Acc) ->
    work(F, Job),
    run(State#{jobs := Jobs, cur := Cur+1}, Acc).

work(F, Job) ->
    Self = self(),
    spawn_link(fun() -> apply(F, [Self, Job]) end),
    ok.

wait() ->
    receive X -> X end.
