erlang学习笔记－运行程序的不同方式
===

在Erlang Shell 里编译运行
---

``` erlang
$ erl
...
1> c(hello).
{ok, hello}
2> hello:start().
Hello world
ok
```

如果想在命令行里执行任意Erlang函数，可以使用 `-eval` 参数:

``` erlang
erl -eval 'io:format("Memory: ~p~n", [erlang:memory(total)]).'\
    -noshell -s init stop
```

在命令提示符界面里编译和运行
---

``` erlang
$ erlc hello.erl
$ erl -noshell -s hello start -s init stop
Hello world
```

作为Escript运行
---

要想让程序直接作为escript脚本运行，而无需编译它们，需要先创建escript文件：

``` erlang
hello

#!/usr/bin/env escript

main(Args) ->
    io:format("Hello world~n")
```

这个文件必须包含一个main(Args)函数。当它从操作系统的shell里面调用时，Args会包含一列用原子表示的命令行参数。这个文件无需编译就可以立即运行：

unix:

``` shell
$ chmod u+x hello
$ .hello
Hello world
```

windows:

``` shell
> escript hello
Hello world
```

使用Makefile自动编译运行
---

简单的erlang makefile 模板：

``` makefile
.SUFFIXES: .erl .beam

.erl.beam:
    erlc -W $<

ERL = erl -boot start_clean

MODS = hello f

all: compile
    ${ERL} -noshell -s hello start -s init stop

compile: ${MODS:%=%.beam}

clean: rm -rf *.beam erl_crash.dump
```