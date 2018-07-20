erlang学习笔记－小工具
===

observer
---

R17版本以上的Erlang才有,将以前版本的tv，appmon，pman，toolbar等工具集成到一起，启动命令为：
> observer:start().

利用这个工具的可以很方便的监控整个应用的状态。应用运行情况、负载、各个进程情况、应用中的表等都可以监控，非常方便。

SASL
---

SASL是Erlang非常实用的日志、消息工具，启动方式：
> application:start(sasl).

启动SASL后再启动应用，应用中的消息、错误报告会被SASL捕获并输出，输出的报告非常详细好用。

在非OTP行为模式的进程中，例如用spawn()方式启动的进程，将不能享受SASL的好处，因为按OTP方式启动的进程做了一些对SASL的准备工作。

使用proc_lib库来启动进程可以按OTP方式启动，也能达到目的：
> proc_lib:spawn().

热更
---

参考资料：https://blog.csdn.net/mycwq/article/details/13290757

1. 第一种热更新方式：
    > {Module, Binary, Filename} = code:get_object_code(Module),

    > code:load_binary(Module, Filename, Binary).

2. 第二种热更新方式：

    > code:purge(Module), code:load_file(Module).
 
3. 第三种热更新方式：

    > code:soft_purge(Module) andalso code:load_file(Module).

Erlang运行时错误
---

参考资料：https://blog.csdn.net/godbaby0312/article/details/51943913