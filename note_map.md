erlang学习笔记－映射组
===

映射组map
---

跟C++的map差不多。

创建一个映射组：

``` erlang
A = #{Key1 Op Val1, Key2 Op Val2, ..., KeyN Op ValN}.
B = A#{Key1 Op Val1, Key2 Op Val2, ..., KeyN Op ValN}.
```

表达式K => Val1有两种用途，一种是现有键K的值更新为新值V，另一种是给映射组添加一个全新的K-V对。

表达式K:=V的作用是将现有的键K的值更新为新值V。而且，这个方式效率高，一般优先使用这个。

映射组的内置函数
---

``` erlang
maps:new()->#{} 返回一个空映射组。

erlang:is_map(M)->bool() 如果M是映射组就返回true，否则返回false。

maps:to_list(M)->[{K1,V1},...,{KN,VN}] 把映射组M里的所有键和值转换成为一个键值列表。

maps:from_list([{K1,V1},...,{KN,VN}])->M 把一个包含键值对的列表转换成映射组。

maps:map_size(Map)->NumberOfEntries 返回映射组的条目数量

maps：is_key(key,Map)->bool() 如果映射组包含一个键为Key的项就返回true，否则返回false。

maps:get(Key,Map)->Val 返回映射组里与Key关联的值，否则抛出一个异常错误。

maps:find(Key,Map)->{ok,Value}|error 返回应这组里与Key关联的值，否则返回error。

maps:keys(Map)->[Key1,...,KeyN] 返回映射组包含的键列表，按升序排列。

maps:remove(Key,M)->M1 返回一个新映射组M1，移除了Key。

maps:without([Key1,...,KeyN],M)->M1 返回一个新的映射组M1，移除了[Key1,...,KeyN]列表的元素。

maps:difference(M1, M2)->M3 M3是M1的复制，移除了M2。

maps:to_json(Map)->Bin 把映射组转换成二进制型，包含了JSON表示的该映射组。

maps:from_json(Bin)->Map 把一个包含JSON数据的二进制型转换成映射组。

maps:safe_from_json(Bin)->Map 把一个包含JSON数据的二进制型转换成映射组。Bin里面的任何原子都必须已经存在，否则会抛异常。
```

映射组可以比较大小，先按长度，然后按元素的键值比较。