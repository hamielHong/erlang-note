Erlang运行时的错误
===

Erlang中的运行错误包括：badarg, badarith, badmatch, function_clause, case_clause, if_clause, undef, noproc, system_limit等。

badarg
---

这个错误很好理解，参数类型错误，传入函数的参数和函数声明要求的参数类型不匹配。

badarith
---

arith，atithmetic的简写，运算错误，例如将一个整数和一个atom相加。

{badmatch, V}
---

模式匹配错误

function_clause
---

从字面意思上看，是函数条款的错误。该错误信息表示找不到匹配的函数。例如，定义一个函数test:add/2，接收两个整数，并返回它们的和。如果传入小数，将找不到匹配的分支，会抛出function_clause。

{case_clause, V}
---

case表达式找不到匹配的分支。一般要把“_”加到最后的分支中，作为容错或者其它。

if_clause
---

Erlang中if表达式是case表达式的一种特殊方式，要求至少有一个分支测试条件的结果为true，否则会引发错误。

undef
---

调用未定义的函数或者模块时，返回该错误信息。

noproc
---

进程不存在，例如gen_server:call一个不存在的进程。

Pid = pid(0, 100, 10).
gen_server:call(Pid, test).
** exception exit: {noproc,{gen_server,call,[<0.100.10>,test]}}
in function gen_server:call/2 (gen_server.erl, line 182)

system_limit
---

超出系统上限，如atom，ets，port，process等。

异常处理
---

在开发中可使用try,catch捕获异常，同时也可以调用erlang:get_stacktrace()获取栈信息，定位错误。

try:
exprs
catch
Class:Reason ->
%% 异常处理代码
%% Class为异常类型，Reason为异常原因
ok
end.

例如：

try:
whatever
catch
Class:Reason ->
io:format("Class:~p, Reason:~p~nstacktrace:~n~p",
[Class, Reason, erlang:get_stacktrace()]),
error
end.