erlang学习笔记－list的使用与优化建议
===

原文：https://blog.csdn.net/mycwq/article/details/32160581

list基本操作
---

``` erlang

1> A = [1, 2, 3, "abc"].
[1,2,3,"abc"]

2> length(A).
4

3> is_list(A).
true

4> list_to_binary(A).
<<1,2,3,97,98,99>>

5> list_to_bitstring(A).
<<1,2,3,97,98,99>>

6> [A1|A2] = A.
[1,2,3,"abc"]

7> A1.
1

8> A2.
[2,3,"abc"]

9> hd(A).
1

10> tl(A).
[2,3,"abc"]

11> B = [0|A].
[0,1,2,3,"abc"]
```

list函数说明
---

常用的lists函数

* **lists:foreach(Fun, List) -> ok**

``` erlang
1> lists:foreach(fun(X) -> io:format("~p~n", [X]) end, [1,2,3]).
1
2
3
ok
```

例子中，通过遍历列表[1,2,3]，把取出的元素给X，直到遍历结束，最后返回ok

* **lists:foldl(Fun, Acc0, List) -> Acc1**

``` erlang
2> lists:foldl(fun(X, Sum) -> X + Sum end, 0, [1,2,3,4,5]).
15
```

例子中，0指定了Sum的初值，通过遍历列表[1,2,3,4,5]，把取出的元素给X，再计算X+Sum，结果带入下次遍历传给 Sum，直到最后一次遍历将X+Sum的结果作为返回值

下面是lists:foldl/3 的实现，实际上是一个尾递归函数

``` erlang
foldl(F, Accu, [Hd|Tail]) ->
    foldl(F, F(Hd, Accu), Tail);
foldl(F, Accu, []) when is_function(F, 2) ->
    Accu.
```