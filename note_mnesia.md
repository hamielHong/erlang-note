erlang学习笔记－Mnesia数据库使用
===

1. 启动 Erlang 时，参数 -mnesia dir '"/tmp/funky"' 指定了 Mnesia 存储数据的目录；
    * 注意，在windows下,如果启动erlang时指定了节点名字，则需要自己先手动创建好目录，不然后续创建数据库会失败，unix下没试验；
    * 没有指定节点名字的情况下能够自动创建目录，原因不明

2. mnesia:create_schema([node()])在本地节点上初始化一个空的 schema；

3. DBMS 通过 mnesia:start()启动；

4. 通过 mnesia:create_table(funky, [])来创建表 funky；

5. mnesia:info()根据数据库的状态来显示信息。