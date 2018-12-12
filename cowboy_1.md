Cowboy学习笔记（一）
===

最近开始看cowboy的源码，在分析源码的同时顺便回顾下opt的各方面内容

当我们需要启动一个otp应用，首先需要定义一个*.app文件，用来描述整个应用的配置，简单看下cowboy.app：

cowboy.app
---

``` erlang
{application, 'cowboy', [
    {description, "Small, fast, modern HTTP server."},
    {vsn, "2.4.0"},
    {modules, ['cowboy','cowboy_app','cowboy_bstr','cowboy_children','cowboy_clear','cowboy_clock','cowboy_compress_h','cowboy_constraints','cowboy_handler','cowboy_http','cowboy_http2','cowboy_loop','cowboy_metrics_h','cowboy_middleware','cowboy_req','cowboy_rest','cowboy_router','cowboy_static','cowboy_stream','cowboy_stream_h','cowboy_sub_protocol','cowboy_sup','cowboy_tls','cowboy_tracer_h','cowboy_websocket']},
    {registered, [cowboy_sup,cowboy_clock]},
    {applications, [kernel,stdlib,crypto,cowlib,ranch]},
    {mod, {cowboy_app, []}},
    {env, []}
]}.
```

各个文件的详细介绍可以查阅《Erlang Design Principles》（《Erlang设计原则》）

这里我们需要关注的是 {mod, {cowboy_app, []}} 这个元组，这里定义了应用启动时首先调用的回调模块和启动参数，这个例子中对应是 cow_app 和 [] ，表示当应用启动时会调用：
> cowboy_app:start(normal, [])

而应用停止时会调用：
> cowboy_app:stop([])

既然应用启动时最先调用的是 cowboy_app:start(normal, []) ，那么接下来我们来看看这个文件：

cowboy_app.erl
---

``` erlang
-module(cowboy_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

-spec start(_, _) -> {ok, pid()}.
start(_, _) ->
    cowboy_sup:start_link().

-spec stop(_) -> ok.
stop(_) ->
    ok.
```

原代码中有一大段注释，我就不搬进来了。可以看到，当应用启动时，会调用 start/2 这个方法，这个方法有两个参数，为了方便解释我把参数名加上：
> start(_Type, _Args) ->

_Type 这个参数的值一般为 normal，只有在接管或故障转移中才会有其他值

_Args 这个参数是从 应用程序配置文件中 {mod, {cowboy_app, <font color="#dd0000">[]</font>}}获取，在这里 _Args = []

> cowboy_sup:start_link()

这行的意思是启动督程，就是监控进程，然后一般在其他子进程会在这棵监控进程树下

剩下一个方法就是 stop(_State)，在应用程序被停止时会被调用，进行必要的一些清理工作。

下面介绍下启动应用程序的方法：

> application:start(cowboy).

停止应用程序的方法：

> application:stop(cowboy).