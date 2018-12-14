erlang学习笔记－supervisor 行为模式
===

重启策略
---

* one_for_one : 把子进程当成各自独立的,一个进程出现问题其它进程不会受到崩溃的进程的影响.该子进程死掉,只有这个进程会被重启

* one_for_all : 如果子进程终止,所有其它子进程也都会被终止,然后所有进程都会被重启.

* rest_for_one:如果一个子进程终止,在这个进程启动之后启动的进程都会被终止掉.然后终止掉的进程和连带关闭的进程都会被重启.

* simple_one_for_one 是one_for_one的简化版 ,所有子进程都动态添加同一种进程的实例

* one-for-one维护了一个按照启动顺序排序的子进程列表,而simple_one_for_one 由于所有的子进程都是同样的(相同的MFA),使用的是字典来维护子进程信息;

``` erlang
init([]) ->
    AChild = {'AName',{'AModule',start_link,[]},
        permanent,2000,worker,['AModule']},
    {ok,{{one_for_all,0,1}, [AChild]}}.
```

ID :supervisor 用来在内部区分specification的,所以只要子进程规格说明之间不重复就可以.

Start : 启动参数{M,F,A}

Restart : 这个进程遇到错误之后是否重启

                permanent:遇到任何错误导致进程终止就会重启
                temporary:进程永远都不会被重启
                transient: 只有进程异常终止的时候会被重启

Shutdown 进程如何被干掉,这里是使用整型值2000的意思是,进程在被强制干掉之前有2000毫秒的时间料理后事自行终止.

            实际过程是supervisor给子进程发送一个exit(Pid,shutdown)然后等待exit信号返回,在指定时间没有返回则将子进程使用exit(Child,kill)
            这里的参数还有 brutal_kill 意思是进程马上就会被干掉
            infinity :当一个子进程是supervisor那么就要用infinity,意思是给supervisor足够的时间进行重启.

Type 这里只有两个值:supervisor worker ; 只要没有实现supervisor behavior的进程都是worker;

                    可以通过supervisor的层级结构来精细化对进程的控制.这个值主要作用是告知监控进程它的子进程是supervisor还是worker

Modules 是进程依赖的模块,这个信息只有在代码热更新的时候才会被用到:标注了哪些模块需要按照什么顺序进行更新;通常这里只需要列出进程依赖的主模块.

如果子进程是supervisor gen_server gen_fsm Module名是回调模块的名称,这时Modules的值是只有一个元素的列表,元素就是回调模块的名称;

如果子进程是gen_event Modules的值是 dynamic;

关于dynamic参数余锋有一篇专门的分析:[Erlang supervisor规格的dynamic行为分析](http://blog.yufeng.info/archives/1455)