# erlang tcp中的socket参数设置

Erlang的inet模块里提供了对Socket进行一系列参数设置的接口 setopts(Socket, Options)。可设置的参数目前有32个(R22)。以下是对几个常用参数的设置做的记录，不定期补充。

> binary

通常我们都指定以二进制的形式来处理Packet的数据。

> {packet, PacketType} </br>
> PacketType : 0 | 1 | 2 | 4 | raw | sunrm | asn1 | cdr | fcgi | line | tpkt | http | httph | http_bin | httph_bin

指定Packet的header大小或者类型，需要根据应用情况明确指定，自己处理协议头则设置为0。

> {active, true | false | once | -32768..32767}

默认值是true。但是由于设置为true的情况下是没有flow control的，所以一般不会使用这个默认值。false的需要每次通过代码精确指定消息数据的接收，视需要而定。通常once是比较常用的设置。

{active,N} 可以看作是对 {active,once} 的一个升级

{% blockquote 没有开花的树 https://blog.csdn.net/mycwq/article/details/24814843%}
我们知道，{active,once}在每次接收到包都要重新设置active选项，才能继续接收erlang的消息通知。实际上，每次设定{active,once}都意味着调用一次epoll_ctl， 如果请求过于频繁，就会有大量的epoll_ctl调用。erlang目前只有一个线程会收割epoll_wait事件，epoll_wait要轮询已经就绪的ctl队列，如果大量的ctl事件将会阻塞了epoll_wait的操作，造成网络处理能力的下降。

那么，我们能不能设定接收N个的socket消息后再执行一次epoll_ctl，这样可以有效减少epoll_ctl的调用，{active,N}就是这样出现的。
{% endblockquote %}

> {reuseaddr, Boolean}

默认是false，一般建议开启，可以复用端口号。同样，这个和系统的设置有关联。

> {delay_send, Boolean}

默认值为false。此时发送给socket的数据会立即尝试通过网络投递，若设置为true，则所有消息会在一开始就进入队列，之后才会发送。对于网络使用繁重但实时性要求没那么高的应用情景来说，设置true会节省网络的占用频度。

> {nodelay, Boolean}

即便少量数据也立即发送。视应用场景而定，一般对于游戏这种类型的应用而言，设置为true。

> {keepalive, Boolean}

默认值为false。设置为true能对一些异常断线的情况进行检测和释放，值得需要注意的是，这个与系统的tcp_keep_alive_time相关。需要将tcp_keep_alive_time设置到一个合理的值。

> {send_timeout, Integer} </br>
> {send_timeout_close, Boolean}

为timeout设定合适的值，send_timeout_close建议设置为true。

> {tos, Integer}

设置IP_TOS参数，这个参数代表了IP包的优先级和QoS选项。和系统的设置有关联。