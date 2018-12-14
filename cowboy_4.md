Cowboy学习笔记（四）
===

在前面的内容中，我们把cowboy本身的启动程序过了一遍，接下来，就要结合例子实际使用cowboy来分析了

源码下面有个 examples 目录，其中都是官方提供给我们使用cowboy的具体例子，我们先从最简单的 hello_world 开始，具体怎么编译运行就不多说了，直接上代码

hello_world.app
---

``` erlang
{application, 'hello_world', [
    {description, "Cowboy Hello World example"},
    {vsn, "1"},
    {modules, ['hello_world_app','hello_world_sup','toppage_handler']},
    {registered, [hello_world_sup]},
    {applications, [kernel,stdlib,cowboy]},
    {mod, {hello_world_app, []}},
    {env, []}
]}.
```

首先还是看下.app文件，毕竟是整个应用的入口，可以看到这一行：

> {applications, [kernel,stdlib,cowboy]}

这是 hello_world 应用的依赖应用，可以看到除了基本的内核和运行库外，还依赖了我们的cowboy，也就是说要先运行cowboy应用，才能顺利启动hello_world

hello_world_app.erl
---

``` erlang
%% Feel free to use, reuse and abuse the code in this file.

%% @private
-module(hello_world_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

%% API.

start(_Type, _Args) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", toppage_handler, []}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(http, [{port, 8080}], #{
        env => #{dispatch => Dispatch}
    }),
    hello_world_sup:start_link().

stop(_State) ->
    ok.
```

第一段 Dispatch 中，定义了我们Web Server的路由规则，路由规则分为**主机规则**和**路径规则**两部分

代码示意如下：

``` erlang
Dispatch = cowboy_router:compile([
    %% {HostMatch, list({PathMatch, Handler, InitialState})}
    {'_', [{'_', my_handler, #{}}]}
]),
%% Name, NbAcceptors, TransOpts, ProtoOpts
cowboy:start_clear(my_http_listener,
    [{port, 8080}],
    #{env => #{dispatch => Dispatch}}
).
```

这个例子中，'_' 定义的是主机规则，所有的规则都可以通过 '_' 来匹配到，与Erlang中的通配规则相同

路径规则是下面的：

`{"/", toppage_handler, []}`

"/" 指路径，所有的路径都必须以 '/'开头

toppage_handler 指的是我们的处理模块的模块名，所有匹配到这个路由的请求，都会发到 toppage_handler 这个模块进行处理

最后的 [] 是处理模块的启动参数

关于路由匹配的详细规则可以参考官方文档：[routing](https://ninenines.eu/docs/en/cowboy/2.6/guide/routing/)

`cowboy_router:compile(Routes)`

这个方法的作用是将我们提供的路由转换为程序容易使用的形式，通过递归的方式解析每一个路由，最后按照顺序放在list中，由此我们可以大胆猜想，在实际路由匹配的过程中，也是将请求与list中的一一进行模式匹配

``` erlang
{ok, _} = cowboy:start_clear(http, [{port, 8080}], #{
        env => #{dispatch => Dispatch}
    }),
```

定义好路由后，需要对端口进行监听，以获取发到指定端口的请求，需要注意的是，启动对端口监听的方法有

> cowboy:start_clear/3 和  cowboy:start_tls/3 两种

前者是启动基于HTTP协议的服务，后者则基于HTTPS协议，我们来看下前者的具体实现：

``` erlang
-spec start_clear(ranch:ref(), ranch:opts(), opts())
	-> {ok, pid()} | {error, any()}.
start_clear(Ref, TransOpts0, ProtoOpts0) ->
	TransOpts1 = ranch:normalize_opts(TransOpts0),
	{TransOpts, ConnectionType} = ensure_connection_type(TransOpts1),
	ProtoOpts = ProtoOpts0#{connection_type => ConnectionType},
	ranch:start_listener(Ref, ranch_tcp, TransOpts, cowboy_clear, ProtoOpts).
```

这里出现了一个没见过的模块**ranch**，其实这是cowboy的依赖库之一，也是开发cowboy的团队开发的，官网的介绍如下:

> Ranch is a socket acceptor pool for TCP protocols.
> Ranch aims to provide everything you need to accept TCP connections with a small code base and low latency while being easy to use directly as an application or to embed into your own.

翻译：

> Ranch是TCP协议的套接字接收器池。
> Ranch旨在提供接受TCP连接所需的一切，具有小代码库和低延迟，同时易于直接用作应用程序或嵌入到您自己的应用程序中。

`ranch:normalize_opts(TransOpts0)` 

TransOpts0这个参数，我们传入的是 **[{port, 8080}]**，这个方法主要是将我们传入的List形式的参数列表转为预设格式的Map，一些socket连接主要的参数将在这一步被抽出构建

接下来也都是构建一些参数，最后调用 **ranch:start_listener/5** 启动监听

有关HTTPS的内容后续再介绍

最后，照例要启动我们的监督者进程：

`hello_cowboy_sup:start_link().`

这个例子中，由于功能比较简单，监督者没有起什么作用，我们跳过他直接看请求的处理模块：

toppage_handler.erl
---

``` erlang
%% @doc Hello world handler.
-module(toppage_handler).

-export([init/2]).

init(Req0, Opts) ->
    Req = cowboy_req:reply(200, #{
        <<"content-type">> => <<"text/plain">>
    }, <<"Hello CowBoy!">>, Req0),
    {ok, Req, Opts}.

```

这就是最简单的一个**handler**模板，它只包含一个方法**init/2**，解释一下它的参数：

Req0 -> 请求信息对象，客户端的请求会被Cowboy构建成一个map对象，我们可以通过这个对象非常方便的获取请求的信息

Opts -> 初始化参数，定义路由时设置

接下来，我们需要构建一个返回消息对象Req，注意，这里服务器的返回消息并不是在**init/2**方法返回 **{ok, Req, Opts}** 时，而是在**cowboy:reply/4**被调用时就立刻返回了，返回的三元组只是代表处理程序成功运行，并把处理的结果返回Cowboy，三元组最后的Opts将作为状态在这个处理程序后续的每个回调中使用