%% 性能测试工具

-module(i).

-export([
    timer_tc/4,
    timer_tc/2
]).

timer_tc(N, M, F, A) ->
    timer:tc(?MODULE, timer_tc, [N, {M, F, A}]).
timer_tc(0, _) -> ok;
timer_tc(N, {M, F, A}) ->
    apply(M, F, A),
    timer_tc(N-1, {M, F, A}).