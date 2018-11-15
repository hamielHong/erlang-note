一些技巧、代码片段
===

长时间定时
---

erlang在处理定时任务时有一个限制，不能定时超过50天；所以在需要定时超过50天时，需要将时间分片后循环处理。

1. 时间分片

    ``` erlang
    normalize(N) ->
    Limit = 49 * 24 * 60 * 60,
    [N rem Limit | lists:duplicate(N div Limit, Limit)].
    ```

    N为需要定时的时间，以秒为单位；Limit为每个分片的最大时间，一般设置为49天；函数返回一个分片的列表。

2. 确定时间转换为秒数

    ``` erlang
    time_to_go(TimeOut = {{_, _, _}, {_, _, _}}) ->
    Now = calendar:local_time(),
    ToGo = calendar:datetime_to_gregorian_seconds(TimeOut) - calendar:datetime_to_gregorian_seconds(Now),
    Secs = if
            ToGo > 0 -> ToGo;
            ToGo =< 0 -> 0
            end,
    normalize(Secs).
    ```

    此函数计算出到{{Year, Month, Day}, {Hour, Minute, Second}}的确定时间的时间间隔，单位为秒，然后调用normalize/1 进行分片。

3. 循环处理

    ``` erlang
    loop(S = #state{server = Server, to_go = [T | Next]}) ->
        receive
            {Server, Ref, cancel} ->
                Server ! {Ref, ok}
        after T * 1000 ->
            if
                Next =:= [] ->
                    Server ! {done, S#state.name};
                Next =/= [] ->
                    loop(S#state{to_go = Next})
            end
        end.
    ```

    对每个时间片进行处理，循环等待，注意时间片的单位是秒，需要 *1000 转换为毫秒进行处理

时间检查
---

检查时期时间是否有效的函数，输入值要满足{{D, M, Y}, {H, Min, S}}格式，可以检查出日期是否存在，时分秒是否合法。

``` erlang
valid_datetime({Date, Time}) ->
    try
        calendar:valid_date(Date) andalso valid_time(Time)
    catch
        error:function_clause -> %% 不符合{{D, M, Y}, {H, Min, S}}格式
            false
    end;

valid_datetime(_) ->
    false.

valid_time({H, M, S}) -> valid_time(H, M, S).
valid_time(H, M, S) when H > 0, H < 24,
                        M >= 0, M < 60,
                        S >= 0, S < 60 -> true;
valid_time(_, _, _) -> false.
```