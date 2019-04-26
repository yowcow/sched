-module(sched).

-export([
    run/3
]).

run(F, List, Max) ->
    run(F, List, Max, 0, []).

run(_, [], _, 0, Acc) -> Acc;
run(F, [], Max, Cur, Acc) ->
    Ret = wait(),
    run(F, [], Max, Cur-1, [Ret | Acc]);
run(F, [V | List], Max, Cur, Acc) when Cur >= Max ->
    Ret = wait(),
    work(F, V),
    run(F, List, Max, Cur, [Ret | Acc]);
run(F, [V | List], Max, Cur, Acc) when Cur < Max ->
    work(F, V),
    run(F, List, Max, Cur+1, Acc).

work(F, V) ->
    Self = self(),
    spawn_link(fun() -> F(Self, V) end),
    ok.

wait() ->
    receive X -> X end.
