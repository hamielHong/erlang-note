erlang学习笔记－ETS
===

ets:match/2
---

ets中的匹配模式有三种：

1. 普通的Erlang项式和已绑定的变量；
2. 单引号内的下划线原子（'_'）。其含义与普通Erlang模式中的下划线相同——用作省略模式或通配符；
3. 形如'$\<integer>'的模式变量（如$1、$2、$3…）。

对于第三种匹配，$\<integer>用于按指定顺序提取匹配到的内容，即在匹配时为匹配到的内容编号，然后再按照编号顺序输出，如：

``` erlang
Tab = ets:new(ets_tab, [named_table, bag]),

ets:insert(Tab, [{rufsen, dog, 7}, {brunte, horse, 5}, {ludde, dog, 5}]),

ets:match(Tab, {'_', '$1', '$2'}).

结果：[[dog,7],[dog,5],[horse,5]]
```

ets:insert/2
---

用法：
> insert(Tab, ObjectOrObjects) -> true

当表中只能存在一个相同键的情况，insert相同键会替换原有值，详细：

向 ETS 表 Tab 插入一个对象数据或者一个对象列表数据 ObjectOrObjects。

如果表是一个 set 类型的表，并且插入的对象数据的键在表里可以匹配得到数据，那么旧的对象数据将会被替换。

如果表是一个 ordered_set 类型的表，并且在表里有跟插入的对象数据有相同的键，那么旧的对象数据将会被替换。

如果插入的对象列表数据里存在多个相同键的情况，并且表是一个 set 类型的表，那么只有一个对象数据可以被插入，不过不确定是哪一个。对于 ordered_set 类型的表，如果键一样时，操作的情况像上面一样。

整个操作都保持着原子性和独立性，即使有多个对象数据插入的情况。