erlang学习笔记－进程监控
===

进程双向监控-Link
---

link方式可以建立进程之间的双向链接关系，当其中一个进程退出时，另一个进程会收到该进程退出的消息。

例子：

``` erlang
-module(test).
-export([start/0]).

start() ->
    Pid = spawn(fun() ->loop() end),
    Pid2 = spawn(fun() ->loop_link(Pid) end),
    io:format("Pid ~p~nPid2 ~p~n", [Pid,Pid2]).

loop_link(Pid) ->
    process_flag(trap_exit, true),
    erlang:link(Pid),
    receive
        Msg ->
            io:format("pid exit: ~p~n", [Msg])
    end.

loop() ->
    process_flag(trap_exit, true),
    receive
        Msg ->
            io:format("pid2 exit: ~p~n", [Msg])
    end.
```

运行结果：

``` erlang
1> test:start().
Pid <0.63.0>
Pid2 <0.64.0>
ok
%% 杀掉Pid进程，进程Pid2收到通知
2> exit(pid(0,63,0),kill).
pid exit: {'EXIT',<0.63.0>,killed}
true

3> test:start().
Pid <0.67.0>
Pid2 <0.68.0>
ok
%% 杀掉Pid2进程，进程Pid收到通知
4> exit(pid(0,68,0),kill).
pid2 exit: {'EXIT',<0.68.0>,killed}
true
```

* 注1：erlang进程默认不捕捉Exit信号，可以使用process_flag(trap_exit, true)改变这一默认行为。

* 注2：解除link监控用erlang:unlink(Pid)

进程单向监控-Monitor
---

Monitor方式则实现进程的单向监控，当被监控进程退出时，监控进程会收到该进程退出的消息。

例子：

``` erlang
-module(test).

-export([start/0]).

start() ->
    Pid = spawn(fun() ->loop() end),
    Pid3 = spawn(fun() ->loop_monitor(Pid) end),
    io:format("Pid ~p~nPid3 ~p~n", [Pid,Pid3]).

loop_monitor(Pid) ->
    _MonitorRef = erlang:monitor(process, Pid),
    receive
        Msg ->
            io:format("pid exit: ~p~n", [Msg])
    end.

loop() ->
    receive
        Msg ->
            io:format("pid3 exit: ~p~n", [Msg])
    end.
```

运行结果：

``` erlang
1> test:start().
Pid <0.39.0>
Pid3 <0.40.0>
ok
%% 杀掉Pid进程，进程Pid3收到通知
2> exit(pid(0,39,0),kill).
pid exit: {'DOWN',#Ref<0.0.0.80>,process,<0.39.0>,killed}
true

3> test:start().
Pid <0.43.0>
Pid3 <0.44.0>
ok
%% 杀掉Pid3进程，进程Pid没有收到通知
4> exit(pid(0,44,0),kill).
true
```

* 注：解除monitor监控用erlang:demonitor(MonitorRef)

如果进程是以 normal 方式退出，erlang将不会发出进程退出通知
---

``` erlang
10> exit(pid(0,70,0), normal).
true
```