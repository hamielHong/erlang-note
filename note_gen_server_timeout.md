erlang学习笔记－genserver的timeout事件总结
===

一、
> start_link（ServerName，M，Args，[{timeout,Time}].

允许gen_server在Time毫秒内完成初始化。

二、
> call（ServerRef，Request，Timeout）

允许客户端进程在Timeout内等到返回结果，默认5s，如果在Timeout内没有结果返回，则客户端进程会因timeout事件而退出，因此当handle_call中有大任务要执行时，通常将该参数设为infinity，允许客户端无限等待结果返回。

三、
>Module：init（Args）->Result={ok,State,Timeout}.

>Module:handle_call(Request,From,State)—>Result={reply，Reply，NewState，Timeout}.

此处的两个返回结果的timeout都是指gen_server在Timeout时间内没有收到一个请求或一条消息时，gen_server会抛出timeout事件退出，此时需要handle_info（timeout，State）来捕获此timeout事件。