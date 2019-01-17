erlang学习笔记－非阻塞的Tcp服务器
===

在研究项目代码时，发现tcp连接时,使用的接口是`prim_inet:async_accept`而不是之前书上学的`gen_tcp:accept`，于是深入研究了下这两个接口的异同和使用。

gen_tcp:accept
---

>Accepts an incoming connection request on a listen socket. Socket must be a socket returned from listen/2. Timeout specifies a timeout value in ms, defaults to infinity. 

gen_tcp模块提供的accept接口是一个阻塞的方法，我们需要指定timeout时间或者默认timeout时间为infinity。

prim_inet:async_accept
---

prim_inet：async_accept是一个非阻塞的异步accept接口。而且是一个非文档函数，只是在内部调用中使用，但是在rabbitmq等很多服务器的accept实现中都用到了这个异步accept的接口。

accept与async_accept的实现
---

查看prim_inet模块关于tcp accept的实现，可以发现，其实gen_tcp:accept的实现是基于async_accept的，gen_tcp:accept会一直阻塞在receive中等待消息。而直接调用async_accept的话，进程会在accept socket后收到消息{inet_async，…}。

{inet_async, L, Ref, {ok, S}}， S就是socket了。

``` erlang
%% prim_inet.erl
%% For TCP sockets only.
%%
accept(L)            -> accept0(L, -1).

accept(L, infinity)  -> accept0(L, -1);
accept(L, Time)      -> accept0(L, Time).

accept0(L, Time) when is_port(L), is_integer(Time) ->
    case async_accept(L, Time) of
    {ok, Ref} ->
        receive 
        {inet_async, L, Ref, {ok,S}} ->
            accept_opts(L, S);
        {inet_async, L, Ref, Error} ->
            Error
        end;
    Error -> Error
    end.
```

回顾下unp上关于unix上异步accept的说法
---

内核中维护的2个队列
---

listen的时候，内核会维护2个队列，一个队列存放未完成的连接，一个队列存放已完成的连接。

这里的未完成的连接指的是客户端第一次握手包被服务器收到的连接，已完成的连接指的是客户端第三次握手包被服务器收到的连接。

阻塞accept 和 非阻塞accept
---

accept的时候，会从已完成的队列获取连接。

如果这是一个阻塞的accept，则在已完成队列是空的情况下会一直等待，直到有连接完成，返回该连接的socket。

如果这是一个非阻塞的accept，则在空的情况下，会马上返回ewouldblock的错误。


说了这些，好像跟上面的有关系，又没什么关系。其实这里还需说明的是 异步、同步、阻塞和非阻塞的关系，鉴于很多书都探讨过，我也不好说什么了。我更倾向于一种说法，在accept的这个情况中，我们使用非阻塞的accept才可以实现异步。为什么呢？

阻塞accept + select =?= 异步accept
---

一般来说异步accept的实现，是使用select\poll或高级的epoll来监测listensokcetfd，一旦它有准备好的连接我们就会收到通知，然后调用accept进行接收。这样看来，就算这个accept的接口是阻塞也没什么影响，listensocketfd返回可读事件，然后我们调用accept去接收。

这确实是个正常的流程，可以这个流程是有风险的，会有一种坑爹的情况导致我们调用accept的时候被阻塞了。

那就是当listensocket返回可读事件的时候，我们刚好没法马上就去调用accept，而在这段时间里（连接完成，尚未accept的时间），客户端这边有刚好发来rst终止连接，服务器这边就会关闭连接。内核中已完成的连接队列就会把这个数据清空了。那么我们的问题就来了，这时候再去accept就会被阻塞了，知道下一个连接到来。显然这个问题的存在，是没法保证我们的 select + blocking accept的实现方案可以满足异步accept的要求的。

所以才会有非阻塞accept，从而真正实现异步accept。
（注：unp一书有关于同步accept和异步accept的详细探讨）

unix上的非阻塞accept 与 erlang实现的async_accept
---

erlang的gen_tcp:accept的实现是基于prim_inet:async_accept的，而prim_inet:async_accept是异步的方式。

所以，erlang底层关于prim_inet:async_accept的实现应该就是上面说的 nonblocking accept + select/poll/epoll 的方式。

为什么要使用async_accept？
---

关于这个问题，大多数的看法是想当然地认为，因为async_accept是异步的，而accept是同步的，异步当然比同步的高级，所以我们使用async_accept。

上面的这种看法，好像蛮对的，但没有具体说原因。网上有一种解释，大概是这样的，当我们的listensocket在同一时刻收到了大量的请求连接时，async_accept可以同时处理这些连接，而gen_tcp:accept只能一个个地处理，然后呢，处理连接的时间需要一次三次握手的时间。那么后面的连接就受不了了，不想等待了。

这种说法，我觉得是错误的，这里的概念是认为accept触发了三次握手。其实accept跟三次握手真没什么联系，上文unp讲到accept只是在已完成连接队列里面获取连接，所以就算不accept，连接照样会建立。只不过这个队列的数量和listen时候的参数backlog有关。

那么为什么要使用async_accept，相比于普通的accept，优势在哪里？其实异步accept和同步accept的区别就在于调用的时候会不会阻塞进程，在非阻塞的情况下进程可以在等待socket的这段时间去做其他事情，而调用阻塞接口的进程只能一直阻塞着等待socket。同一时间有大量连接存在，也就说已完成连接队列里有多个连接的时候，不管是async_accept还是同步accept，都是一次接收一个连接的。

假设有两种实现：

实现1是，一个accept进程调用gen_tcp:accept，等待获取连接，一旦连接成功该socket就交给业务进程处理，然后accept进程继续阻塞，等待下一次获取连接。

实现2是，一个accept进程调用prim_inet:async_accept，然后等待处理连接消息，该消息的处理方式：也是把socket交给业务进程处理，然后在没有收到连接消息的时候，他也可以处理其他消息。

实现1和实现2的区别，就在于accept进程有没有空闲时间出来处理其他的业务，实现2的accept除了处理{inet_async…}消息，也可以匹配处理其他消息。

但是当同一时刻大量连接到来时，实现2相比实现1是否能够更能提高accept的效率呢，我认为是不可以的。因为实现1和实现2一样得从内核的队列中获取连接。但是这一点只是个猜想，还需验证。

多进程同时accept
---

上面说到的问题，当同一时刻有大量连接到来，怎么更高效地accept这个连接呢？毕竟上文只是说了单个进程使用async_accept并不能提高这种情况的处理效率。其实这个情况我们可以创建多个进程同时accept socket。这个方案也是很常见的，ranch（erlang的网络accept库）就是使用了这一方案。ranch的做法大概是由ranch_acceptor_sup开始gen_tcp:listen，并把得到的listensokcet传递给多个子进程ranch_acceptor，然后每个ranch_acceptor都开始阻塞在gen_tcp:accept(listensocket, Timeout)上，然后在同时大量连接到来的时候，这些进程会有序地一个个accept到socket。

说到多进程accept不可避免的必须提到“惊群问题”，惊群问题已经在linux内核中解决了。那么我还是不说了哈哈。其实我们一定很好奇，那么多个ranch_acceptor都阻塞在accept中，但只有一个连接到来的时候，谁会accept成功返回，其他进程又会怎样。

这种方式在早期的erlang是不安全的，但R11B03 版本之后，erlang做了改进，允许多个进程同时监听同一个socket。

erlang R11B03 更新日志

> OTP-6416 gen_tcp now allows for several processes to issue accept calls to the same listen-socket simultaniously. The different accepting processes will get connected sockets on a first-come-first-served basis.

当多个进程同时accept一个socket，erlang 内部将使用队列保存acceptor信息，以先来先服务的原则将新的连接关联到acceptor，再给acceptor投递消息 {inet_async, L, Ref, Result}。

具体看\erts\emulator\drivers\common\inet_drv.c 的 tcp_inet_ctl 函数，erlang对socket的处理都集中在inet_drv.c

erlang配合{backlog, N}，监听新连接效果更佳

backlog是erlang用来设置socket等待连接队列。N为队列的长度，默认值5，显然太小了

实验
---

为了验证关于async_accept的种种解释和说法，参考 [Building a Non-blocking TCP server using OTP principles](https://erlangcentral.org/wiki/Building_a_Non-blocking_TCP_server_using_OTP_principles) 实现了一个基于 async_accept 的非阻塞tcp server，相关代码都放在 /async_server 目录下

目前这个服务器还只是使用单一进程来 accept，之后再继续试验使用多个进程进行accept，看看是否真的能提高效率

参考链接
---

[prim_inet:async_accept](https://blog.csdn.net/aaaajw/article/details/51725244)

[erlang如何有效监听大量并发连接](https://blog.csdn.net/mycwq/article/details/27108911)

[gen_tcp async accept大致流程](https://bachmozart.iteye.com/blog/482507)

[Erlang socket 工作原理](https://blog.csdn.net/yuanlin2008/article/details/8277404)

[Building a Non-blocking TCP server using OTP principles](https://erlangcentral.org/wiki/Building_a_Non-blocking_TCP_server_using_OTP_principles)

[基于Erlang OTP构建一个TCP服务器](http://www.blogjava.net/yongboy/archive/2012/10/24/390185.html)