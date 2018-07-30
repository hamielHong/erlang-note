erlang学习笔记－记录使用
===

一、定义记录
---

应将记录定义在扩展名为.hrl的文件中，其定义格式为：

> －record(thing,{keya=defaulta,keyb}).

其中，thing为记录的名称；keya、keyb为记录中的字段名，其中keya带有一个默认值defaulta。

二、读取记录的定义
---

在shell中用
> rr("filename.hrl")

来读取记录定义，然后就可以使用了。

三、创建记录
---

基本格式：

> X=#thing{keyb=3}.

其中没有指定keya的值，会默认为defaulta。

四、更新记录
---

格式为：

> X2＝X#thing{keyb=5}.

即创建一个副本X2，同时更新了keyb的值为5。

五、使用记录中字段的值
---

1、匹配法

> #thing{keya=Keya,keyb=V} = X

则Keya的值为defaulta,V的值为3。

在函数定义中，就要采用这种方式来获取记录中字段的值。

* 注意：反过来匹配是不可行的，编译器会认为这个操作是在创建记录，而Keya和V都是未定义的变量，会报错，如：
> X = #thing{keya=Keya,keyb=V}

* 在模式匹配中，会先计算等式右边的值，等式左边称为模式，右边称为表达式

2、用点语法直接使用

> X#thing.keyb

则直接使用X的keyb字段的值3。

六、其它
---

记录是元组的伪装，当使用rf("thing")释放记录之后，已创建的记录就会成为元组。如以上X就会显示为：

> {thing,defaulta,3}

如果需要在erlang程序文件中使用记录文件，则用以下方式导入：

> -include("mess_interface.hrl").