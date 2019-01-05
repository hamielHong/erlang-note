erlang学习笔记－ io:format 总结
===

io:format/2
---

按照指定的格式把数据写入到输出端上

用法：

``` erlang
format(Format, Data) -> ok
```

内部实现：

``` erlang
-spec format(Format, Data) -> 'ok' when
    Format :: format(),
    Data :: [term()].

format(Format, Args) ->
    format(default_output(), Format, Args).
```

把参数 Data 里的每一项根据 Format 的输出格式写入到默认输出端。

其中参数 Format 的写法形式是 "~F.P.PadModC"

* F表示输出长度和格式
* P表示输出精度
* Pad表示输出填充字符
* Mod控制类型的修饰
* C表示控制类型

比如常见的格式参数有：

~c 表示只接受 ASCII 码所表示的数字，例如下面 10.5c 表示打印 5 次 a，长度是 10 的字符串（长度不足时空格表示）：

``` erlang
io:format("|~10.5c|", [$a]).

运行代码：| aaaaa|
```

~f 表示浮点数输出，默认保留 6 为小数，下面的 15.2f 表示数字总共占 15 位，小数保留 2 位（保留时四舍五入）

``` erlang
Pi = math:pi(),
io:format("Pi is ~15.2f", [Pi]).

运行代码：Pi is 3.14
```

~s 表示按字符串形式输出：

``` erlang
io:format("abcdefg~s", [hijklmn]).

运行代码：abcdefghijklmn
```

~t 表示按Unicode输出

``` erlang
Chinese = unicode:characters_to_list("中文测试"),
io:format("~ts~n", [Chinese]).

运行代码：[228,184,173,230,150,135,230,181,139,232,175,149,10]
```

~w 表示输出一个 Erlang term

``` erlang
io:format("List is ~w", [[97]]).

运行代码：List is [97]
```

~p 与 ~w 类似，不过 ~p 的输出是有格式的，默认一行的显示的最大长度为80，则多行时会自动换行

``` erlang
L = lists:seq(1, 100),
io:format("L is ~p", [L]).

运行代码：
L is [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27, 28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51, 52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75, 76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99, 100]
```

~n 表示换行符

``` erlang
io:format("newline1~n newline2~n", []).

运行代码：newline1 newline2
```

io:format/3
---

按照指定的格式把数据写入到输出端上

用法：

``` erlang
format(IoDevice, Format, Data) -> ok
```

内部实现：

``` erlang
-spec format(IoDevice, Format, Data) -> 'ok' when
      IoDevice :: device(),
      Format :: format(),
      Data :: [term()].

format(Io, Format, Args) ->
    o_request(Io, {format,Format,Args}, format).
```

跟 io:format/2 唯一的区别就是多了一个输出端 IoDevice 参数，不用原来默认的输出端作为输出载体，其他参数 Format 和 Data 跟 io:format/2 一样。

这个函数需要指定一个输出端（通常，调用 file:open/2 会返回一个输出端）然后作为参数传入给函数，假设当前目录下有个 test.txt 的文件

``` erlang
{ok, IoDevice} = file:open("test.txt", write),
io:format(IoDevice, "~s~n", ["Just a test!"]).

运行代码：ok
```