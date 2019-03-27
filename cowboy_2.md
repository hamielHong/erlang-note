Cowboy学习笔记（二）
===

上一篇简单介绍了Cowboy启动时会用到的2个文件，分别是cowboy.app（配置文件）和 cowboy_app.erl （应用启动入口文件）

接下来，我们继续深入，上篇提到cowboy_app.erl 的 start/2 方法：

``` erlang
start(_, _) ->
    cowboy_sup:start_link().
```

那么我们来看下 cowboy_sup 这个模块：

cowboy_sup.erl
---

``` erlang
-module(cowboy_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec init([])
    -> {ok, {{supervisor:strategy(), 10, 10}, [supervisor:child_spec()]}}.
init([]) ->
    Procs = [{cowboy_clock, {cowboy_clock, start_link, []},
        permanent, 5000, worker, [cowboy_clock]}],
    {ok, {{one_for_one, 10, 10}, Procs}}.
```

这个模块就是otp中另一大行为模式—— supervisor （监督者）的实现，看下 supervisor 的简单介绍(摘抄自Erlang设计原则)：

> 督程负责启动、停止和监视它的子进程。督程的基本思想是它要保持它的子进程有效，必要的时候可以重启他们。
> 要启动和监视的子进程由一个 子进程规格 的列表来指定。子进程按照在这个列表中的顺序启动，并且按照相反的顺序终止。

当我们调用 supervisor:start_link({local, ?MODULE}, ?MODULE, []). 这个进程会回调自己的 init/1 方法进行进程的初始化，init/1 需要返回一个元组作为初始化的参数，这里对返回的元组中一些字段解释一下：

``` erlang
%% 返回元组格式
{ok, {SupFlags, ChildSpec}}

SupFlags = {Strategy, Intensity, Period}

Strategy = one_for_one | one_for_all | rest_for_one |simple_one_for_one
Intensity = integer() >= 0
Period = integer() >= 1

ChildSpec = {Id, StartFunc, Restart, Shutdown, Type, Modules}

Id = term()
StartFunc = {M, F, A}
    M = F = atom()
    A = [term()]
Restart = permanent | transient | temporary
Shutdown = brutal_kill | integer() >=0 | infinity
Type = worker | supervisor
Modules = [Module] | dynamic
Module = atom()
```

SupFlags 进程策略：

* Strategy 重启模式

  * one_for_one 如果子进程终止，只有这个进程会被重启
  * one_for_all 如果子进程终止，所有子进程都会被重启
  * rest_for_one 如果子进程终止，在这个进程重启后，其他所有子进程会被重启
  * simple_one_for_one 简化one_for_one，所有子进程都动态添加同一种进程的实例
  * one_for_one 和 simple_one_for_one 最大的区别在于：one_for_one 用一个列表储存了所有要启动的子进程，并按顺序启动。而simple_one_for_one 用一个进程字典定义所有子进程。所以当一个监督者拥有很多子进程时，遇到进程崩溃，simple_one_for_one 效率会快很多

* Intensity 最多允许重启次数 0表示不重启

* Period 限定重启时间，单位：s（秒）

* 结合起来看就是，在多长时间内（Period）最多允许重启几次（Intensity），重启的方式为（Strategy），Period 和 Intensity 的设定意义在于限制最大重启频率，这是为了避免反复重启进入死循环。

Procs 子进程相关：

* Id 是督程内部用于标识子进程规范的名称。

* StartFunc 定义了用于启动子进程的很难书调用。它是一个模块.函数.参数的元组，与 apply(M, F, A) 用的一样。

* Restart定义了一个被终止的子进程要在何时被重启：

  * permanent 子进程总会被重启。
  * temporary 子进程从不会被重启。
  * transient 子进程只有当其被异常终止时才会被重启，即，退出理由不是 normal 。

* Shutdown定义了一个子进程应如何被终止。

  * brutal_kill 表示子进程应使用 exit(Child, kill) 进行无条件终止。
  * integer() >=0 一个整数超时值表示督程先通过调用 exit(Child, shutdown) 告诉子进程要终止了，然后等待其返回退出信号。如果在指定的事件内没有接受到任何退出信号，那么使用 exit(Child, kill) 无条件终止子进程。
  * infinity 如果子进程是另外一个督程，那么应该设置为 infinity 以给予子树足够的时间关闭。

* Type 指定子进程是督程还是佣程。

* Modules 应该为只有一个元素的列表 [Module]，其中 Module 是回调模块的名称，如果子进程是督程、gen_server或者gen_fsm。如果子进程是一个gen_event，那么 Modules 应为 dynamic 。 在升级和降级过程中发布处理器将用到这个信息。

结合上面的解释，我们就可以理解 cowboy_sup.erl 中 init/1 方法中代码的意思了：

``` erlang
Procs = [{cowboy_clock, {cowboy_clock, start_link, []},
    permanent, 5000, worker, [cowboy_clock]}],
{ok, {{one_for_one, 10, 10}, Procs}}.
```

表示这个cowboy_sup监控树下，会启动一个 cowboy_clock 模块，启动的模式是 permanent 即总会重启，强制结束的等待时间设置为5000，worker表示启动的子进程是一个佣程

重启的策略是 one_for_one 一对一模式，并且在 10s 中最多允许重启 10 次