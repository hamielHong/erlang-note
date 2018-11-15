erlang学习笔记－gen_server call
===

一般我们写 gen_server handle_call会这么写

``` erlang
handle_call(call, _From, State) ->
    Result = compute(State),  %%compute 或许为一个耗时操作
    {reply, Result  State}.
```

简单的通过计算返回state中的某个数据
如果compute为一个耗时操作，那么整个进程可能会堵塞

当操作不会改变gen_server  state数据时

可以这么写

``` erlang
handle_call(call, From, State) ->
    spawn(fun() ->
        Result = compute(State),
            gen_server:replay(From, Result)
    end),
    {noreply, State}.
```

将耗时操作spawn一个进程去操作，call直接返回 noreplay，通过gen_server:replay 进行返回数据

这样gen_server在spawn进程后就会被释放，提高gen_server承载

同时，由于传进计算的state就是当时拿到的数据拷贝，所以也不影响队列。

不过，当需要改变state数据时就不能这么干了。。

附上gen_server:replay文档

``` erlang
reply(Client, Reply) -> Result

Types:
Client - see below
Reply = term()
Result = term()

This function can be used by a gen_server to explicitly send a reply to a client that called call/2,3 or multi_call/2,3,4, 
when the reply cannot be defined in the return value of Module:handle_call/3.

Client must be the From argument provided to the callback function.
 Reply is an arbitrary term, which will be given back to the client as the return value of call/2,3 or multi_call/2,3,4.

The return value Result is not further defined, and should always be ignored.
```