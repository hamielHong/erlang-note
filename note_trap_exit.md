erlang学习笔记－trap_exit
===

Erlang除了提供try catch，catch等语法，还支持link和monitor两种监控进程的机制，使得所有进程可以连接起来，组成一个整体。除了这样，erlang还提供trap_exit方法让进程主动捕获异常退出消息，避免发生异常时被系统关闭。

trap_exit说明
erlang设计上有速错(fast on fail)的原则，就是让问题快速暴露，迅速停止这个进程的运行，以避免这个进程错误执行造成更多的错误。但是在业务层需要容错，所以erlang就把选择权交给用户，通过process_flag(trap_exit,true)来避免进程被异常关闭。

``` erlang
-module(test).
-compile(export_all).
start() ->
    Pid = spawn(
        fun() ->
            process_flag(trap_exit,true),
            do_loop()
        end),
    register(test, Pid).

do_loop() ->
    receive
        Msg ->
            io:format("recv ~w~n", [Msg])
    end,
    do_loop().

```

测试如下：

``` erlang
1> test:start().
true
2> exit(whereis(test),normal).
recv {'EXIT',<0.35.0>,normal}
true
3> whereis(test).
<0.33.0>
```

这里可以看出进程被exit/2 后并没有挂掉。
在exit/2的文档可以找到这么一段话：

``` erlang
exit(Pid, Reason) -> true

If Pid is not trapping exits, Pid itself will exit with exit reason Reason. If Pid is trapping exits, the exit signal is transformed into a message {'EXIT', From, Reason} and delivered to the message queue of Pid. From is the pid of the process which sent the exit signal.
```

意思是，如果进程没有trap_exit 的话，exit/2 就会直接关闭进程；如果设了trap_exit，进程不会被关闭，而是收到一条退出消息：{'EXIT', From, Reason}

trap_exit临界点
但是，有个问题，如果我们设了trap_exit，但是想强制关闭进程怎么办？

erlang还是给了我们答案：

``` erlang
If Reason is the atom kill, that is if exit(Pid, kill) is called, an untrappable exit signal is sent to Pid which will unconditionally exit with exit reason killed.
```

当 exit/2 的Reason是kill时，进程就算设了trap_exit 也无法阻止进程被关闭

``` erlang
5> exit(whereis(test),kill).
true
6> whereis(test).
undefined
```

所以，kill被用来强制杀进程。以上也就是为什么gen_server进程被exit(Pid, kill) 时，terminate/2 无法捕获到进程退出消息的原因。

exit信号
再来说下trap_exit与进程退出信号的关系：

![avatar](https://img-blog.csdn.net/20140321180558078?watermark/2/text/aHR0cDovL2Jsb2cuY3Nkbi5uZXQvbXljd3E=/font/5a6L5L2T/fontsize/400/fill/I0JBQkFCMA==/dissolve/70/gravity/NorthEast)

除了前面讲到的，还可以看出，不管进程是否有设置 trap_exit，进程被kill时link进程都会收到这个进程的退出消息。只要进程退出，link进程都会收到这个进程退出消息（normal除外），Supervisor就是这样构建起来的。那么，normal可以用来杀掉Supervisor的进程。

拓展链接：[Erlang/OTP 监督者（Supervisor）](https://blog.csdn.net/mycwq/article/details/12690093)、[erlang进程监控：link和monitor](http://blog.csdn.net/mycwq/article/details/13171117)