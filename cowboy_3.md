Cowboy学习笔记（三）
===

上篇讲到 cowboy_sup 启动时，会启动子进程 cowboy_clock，这一篇我们就来看下这个模块

cowboy_clock.erl
---

```erlang
%% While a gen_server process runs in the background to update
%% the cache of formatted dates every second, all API calls are
%% local and directly read from the ETS cache table, providing
%% fast time and date computations.
-module(cowboy_clock).
-behaviour(gen_server).

%% API.
-export([start_link/0]).
-export([stop/0]).
-export([rfc1123/0]).
-export([rfc1123/1]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
    universaltime = undefined :: undefined | calendar:datetime(),
    rfc1123 = <<>> :: binary(),
    tref = undefined :: undefined | reference()
}).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec stop() -> stopped.
stop() ->
    gen_server:call(?MODULE, stop).

%% When the ets table doesn't exist, either because of a bug
%% or because Cowboy is being restarted, we perform in a
%% slightly degraded state and build a new timestamp for
%% every request.
-spec rfc1123() -> binary().
rfc1123() ->
    try
        ets:lookup_element(?MODULE, rfc1123, 2)
    catch error:badarg ->
        rfc1123(erlang:universaltime())
    end.

-spec rfc1123(calendar:datetime()) -> binary().
rfc1123(DateTime) ->
    update_rfc1123(<<>>, undefined, DateTime).

%% gen_server.

-spec init([]) -> {ok, #state{}}.
init([]) ->
    ?MODULE = ets:new(?MODULE, [set, protected,
        named_table, {read_concurrency, true}]),
    T = erlang:universaltime(),
    B = update_rfc1123(<<>>, undefined, T),
    TRef = erlang:send_after(1000, self(), update),
    ets:insert(?MODULE, {rfc1123, B}),
    {ok, #state{universaltime=T, rfc1123=B, tref=TRef}}.

-type from() :: {pid(), term()}.
-spec handle_call
    (stop, from(), State) -> {stop, normal, stopped, State}
    when State::#state{}.
handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

-spec handle_cast(_, State) -> {noreply, State} when State::#state{}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(any(), State) -> {noreply, State} when State::#state{}.
handle_info(update, #state{universaltime=Prev, rfc1123=B1, tref=TRef0}) ->
    %% Cancel the timer in case an external process sent an update message.
    _ = erlang:cancel_timer(TRef0),
    T = erlang:universaltime(),
    B2 = update_rfc1123(B1, Prev, T),
    ets:insert(?MODULE, {rfc1123, B2}),
    TRef = erlang:send_after(1000, self(), update),
    {noreply, #state{universaltime=T, rfc1123=B2, tref=TRef}};
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(_, _) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(_, State, _) -> {ok, State} when State::#state{}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal.

-spec update_rfc1123(binary(), undefined | calendar:datetime(),
    calendar:datetime()) -> binary().
update_rfc1123(Bin, Now, Now) ->
    Bin;
update_rfc1123(<< Keep:23/binary, _/bits >>,
        {Date, {H, M, _}}, {Date, {H, M, S}}) ->
    << Keep/binary, (pad_int(S))/binary, " GMT" >>;
update_rfc1123(<< Keep:20/binary, _/bits >>,
        {Date, {H, _, _}}, {Date, {H, M, S}}) ->
    << Keep/binary, (pad_int(M))/binary, $:, (pad_int(S))/binary, " GMT" >>;
update_rfc1123(<< Keep:17/binary, _/bits >>, {Date, _}, {Date, {H, M, S}}) ->
    << Keep/binary, (pad_int(H))/binary, $:, (pad_int(M))/binary,
        $:, (pad_int(S))/binary, " GMT" >>;
update_rfc1123(<< _:7/binary, Keep:10/binary, _/bits >>,
        {{Y, Mo, _}, _}, {Date = {Y, Mo, D}, {H, M, S}}) ->
    Wday = calendar:day_of_the_week(Date),
    << (weekday(Wday))/binary, ", ", (pad_int(D))/binary, Keep/binary,
        (pad_int(H))/binary, $:, (pad_int(M))/binary,
        $:, (pad_int(S))/binary, " GMT" >>;
update_rfc1123(<< _:11/binary, Keep:6/binary, _/bits >>,
        {{Y, _, _}, _}, {Date = {Y, Mo, D}, {H, M, S}}) ->
    Wday = calendar:day_of_the_week(Date),
    << (weekday(Wday))/binary, ", ", (pad_int(D))/binary, " ",
        (month(Mo))/binary, Keep/binary,
        (pad_int(H))/binary, $:, (pad_int(M))/binary,
        $:, (pad_int(S))/binary, " GMT" >>;
update_rfc1123(_, _, {Date = {Y, Mo, D}, {H, M, S}}) ->
    Wday = calendar:day_of_the_week(Date),
    << (weekday(Wday))/binary, ", ", (pad_int(D))/binary, " ",
        (month(Mo))/binary, " ", (integer_to_binary(Y))/binary,
        " ", (pad_int(H))/binary, $:, (pad_int(M))/binary,
        $:, (pad_int(S))/binary, " GMT" >>.

%% Following suggestion by MononcQc on #erlounge.
-spec pad_int(0..59) -> binary().
pad_int(X) when X < 10 ->
    << $0, ($0 + X) >>;
pad_int(X) ->
    integer_to_binary(X).

-spec weekday(1..7) -> <<_:24>>.
weekday(1) -> <<"Mon">>;
weekday(2) -> <<"Tue">>;
weekday(3) -> <<"Wed">>;
weekday(4) -> <<"Thu">>;
weekday(5) -> <<"Fri">>;
weekday(6) -> <<"Sat">>;
weekday(7) -> <<"Sun">>.

-spec month(1..12) -> <<_:24>>.
month( 1) -> <<"Jan">>;
month( 2) -> <<"Feb">>;
month( 3) -> <<"Mar">>;
month( 4) -> <<"Apr">>;
month( 5) -> <<"May">>;
month( 6) -> <<"Jun">>;
month( 7) -> <<"Jul">>;
month( 8) -> <<"Aug">>;
month( 9) -> <<"Sep">>;
month(10) -> <<"Oct">>;
month(11) -> <<"Nov">>;
month(12) -> <<"Dec">>.

%% Tests.

-ifdef(TEST).
update_rfc1123_test_() ->
    Tests = [
        {<<"Sat, 14 May 2011 14:25:33 GMT">>, undefined,
            {{2011, 5, 14}, {14, 25, 33}}, <<>>},
        {<<"Sat, 14 May 2011 14:25:33 GMT">>, {{2011, 5, 14}, {14, 25, 33}},
            {{2011, 5, 14}, {14, 25, 33}}, <<"Sat, 14 May 2011 14:25:33 GMT">>},
        {<<"Sat, 14 May 2011 14:25:34 GMT">>, {{2011, 5, 14}, {14, 25, 33}},
            {{2011, 5, 14}, {14, 25, 34}}, <<"Sat, 14 May 2011 14:25:33 GMT">>},
        {<<"Sat, 14 May 2011 14:26:00 GMT">>, {{2011, 5, 14}, {14, 25, 59}},
            {{2011, 5, 14}, {14, 26,  0}}, <<"Sat, 14 May 2011 14:25:59 GMT">>},
        {<<"Sat, 14 May 2011 15:00:00 GMT">>, {{2011, 5, 14}, {14, 59, 59}},
            {{2011, 5, 14}, {15,  0,  0}}, <<"Sat, 14 May 2011 14:59:59 GMT">>},
        {<<"Sun, 15 May 2011 00:00:00 GMT">>, {{2011, 5, 14}, {23, 59, 59}},
            {{2011, 5, 15}, { 0,  0,  0}}, <<"Sat, 14 May 2011 23:59:59 GMT">>},
        {<<"Wed, 01 Jun 2011 00:00:00 GMT">>, {{2011, 5, 31}, {23, 59, 59}},
            {{2011, 6,  1}, { 0,  0,  0}}, <<"Tue, 31 May 2011 23:59:59 GMT">>},
        {<<"Sun, 01 Jan 2012 00:00:00 GMT">>, {{2011, 5, 31}, {23, 59, 59}},
            {{2012, 1,  1}, { 0,  0,  0}}, <<"Sat, 31 Dec 2011 23:59:59 GMT">>}
    ],
    [{R, fun() -> R = update_rfc1123(B, P, N) end} || {R, P, N, B} <- Tests].

pad_int_test_() ->
    Tests = [
        { 0, <<"00">>}, { 1, <<"01">>}, { 2, <<"02">>}, { 3, <<"03">>},
        { 4, <<"04">>}, { 5, <<"05">>}, { 6, <<"06">>}, { 7, <<"07">>},
        { 8, <<"08">>}, { 9, <<"09">>}, {10, <<"10">>}, {11, <<"11">>},
        {12, <<"12">>}, {13, <<"13">>}, {14, <<"14">>}, {15, <<"15">>},
        {16, <<"16">>}, {17, <<"17">>}, {18, <<"18">>}, {19, <<"19">>},
        {20, <<"20">>}, {21, <<"21">>}, {22, <<"22">>}, {23, <<"23">>},
        {24, <<"24">>}, {25, <<"25">>}, {26, <<"26">>}, {27, <<"27">>},
        {28, <<"28">>}, {29, <<"29">>}, {30, <<"30">>}, {31, <<"31">>},
        {32, <<"32">>}, {33, <<"33">>}, {34, <<"34">>}, {35, <<"35">>},
        {36, <<"36">>}, {37, <<"37">>}, {38, <<"38">>}, {39, <<"39">>},
        {40, <<"40">>}, {41, <<"41">>}, {42, <<"42">>}, {43, <<"43">>},
        {44, <<"44">>}, {45, <<"45">>}, {46, <<"46">>}, {47, <<"47">>},
        {48, <<"48">>}, {49, <<"49">>}, {50, <<"50">>}, {51, <<"51">>},
        {52, <<"52">>}, {53, <<"53">>}, {54, <<"54">>}, {55, <<"55">>},
        {56, <<"56">>}, {57, <<"57">>}, {58, <<"58">>}, {59, <<"59">>}
    ],
    [{I, fun() -> O = pad_int(I) end} || {I, O} <- Tests].
-endif.
```

先看一下对这个模块的描述：

注释
---

``` erlang
%% While a gen_server process runs in the background to update
%% the cache of formatted dates every second, all API calls are
%% local and directly read from the ETS cache table, providing
%% fast time and date computations.
```

自己翻译一下：进程在后台运行时每秒更新日期格式化的缓存，所有API调用会从ETS缓存中直接读取，以提供快速计算时间和日期功能。

有意思，看来cowboy中所有的时间相关的调用都会涉及到这个模块，其实这个设计思路我们之前都看过，我们游戏中的时间也是像这样封装了一个模块，内部同样是用ETS储存时间，以应对频繁的时间计算请求，之前我还质疑过这样做的性能消耗，看来确实是一个最佳实践。

不过游戏中的那个时间缓存刷新率是 100ms，而cowboy中是 1s，我想主要是因为游戏中的时间计算更加频繁，需要的精度更小，之前杭爷举的一个例子就是怪物AI，怪物的AI请求都是以整百毫秒为单位，需要更小一些的时间精度。

定义
---

回到 cowboy_clock 可以发现，首先这个模块定义的行为模式是：

> -behaviour(gen_server).

gen_server 我们已经非常熟悉了，下面的接口导出和回调函数导出也就不一一解释了，直接看到记录的定义：

``` erlang
-record(state, {
    universaltime = undefined :: undefined | calendar:datetime(),
    rfc1123 = <<>> :: binary(),
    tref = undefined :: undefined | reference()
}).
```

注意一下，这里可能跟我们平时写的代码有些区别，用到了 :: 字段类型 进行类型声明

关于类型声明，主要是用于使用 dialyzer 对代码进行静态分析，在编译期就可以发现代码中的很多错误，现在游戏项目中还没有使用 dialyzer 所以也就没有声明，个人建议养成类型声明的习惯

API
---

接下来看看API

start_link/0 和 stop/0 都没什么看的，没有什么骚操作

``` erlang
%% When the ets table doesn't exist, either because of a bug
%% or because Cowboy is being restarted, we perform in a
%% slightly degraded state and build a new timestamp for
%% every request.
-spec rfc1123() -> binary().
rfc1123() ->
    try
        ets:lookup_element(?MODULE, rfc1123, 2)
    catch error:badarg ->
        rfc1123(erlang:universaltime())
    end.

-spec rfc1123(calendar:datetime()) -> binary().
rfc1123(DateTime) ->
    update_rfc1123(<<>>, undefined, DateTime).
```

rfc1123/0 和 rfc1123/1 这两个API很奇怪，命名不是我们常见的用接口的操作的英文进行命名，其实 rfc1123 指的是一个国际通用的时间格式规范 RFC-1123，大概就是这样表示日期：

ddd,dd MMM yyyy,HH':'mm':'ss 'GMT'

所以 rfc1123/0 的作用是返回当前的日期和时间，并格式化为符合RFC-1123规范的表示形式

> ets:lookup_element(?MODULE, rfc1123, 2)

这里是从储存的ets中取键为 rfc123 的对象的第 2 个元素，也就是我们要的时间数据

rfc1123/1 是传入一个时间参数，将该时间转化为RFC-1123格式后返回

查阅资料发现，在旧的版本对 rfc1123/1 的注释中，还有这行：

``` erlang
%% This format is used in the <em>'Set-Cookie'</em> header sent with HTTP responses.
```

大概意思就是,这种格式化，用于发送 http responses 时放在header的 'Set-Cookie' 中.

注意这里开始，使用了函数规范

> -spec rfc1123() -> binary().
> -spec rfc1123(calendar:datetime()) -> binary().

函数规范定义与类型声明相同，都可以用来帮助dialyzer进行静态分析，同时函数规范定义还可以帮助我们使用erlang自带的工具生成文档

这里的定义表示，这两个函数的返回值，都是 binary 即二进制类型，使用时要进行注意

回调函数
---

init/1 gen_server的初始化回调方法

``` erlang
-spec init([]) -> {ok, #state{}}.
init([]) ->
	?MODULE = ets:new(?MODULE, [set, protected,
		named_table, {read_concurrency, true}]),
	T = erlang:universaltime(),
	B = update_rfc1123(<<>>, undefined, T),
	TRef = erlang:send_after(1000, self(), update),
	ets:insert(?MODULE, {rfc1123, B}),
	{ok, #state{universaltime=T, rfc1123=B, tref=TRef}}.
```

这个方法创建一个命名ETS表，这种创建的写法很有意思，当创建失败时，返回值无法匹配表名，会直接导致进程崩溃，交给监督者重启

接下来就是 erlang:universaltime() 获取当前系统时间，然后把系统时间转为 RFC-1123 的二进制格式

再通过 erlang:send_after 延迟 1000ms 发一条 update 的消息给自己

最后，时间存入ETS表，并存入State中返回

handle_info/2

``` erlang
-spec handle_info(any(), State) -> {noreply, State} when State::#state{}.
handle_info(update, #state{universaltime=Prev, rfc1123=B1, tref=TRef0}) ->
	%% Cancel the timer in case an external process sent an update message.
	_ = erlang:cancel_timer(TRef0),
	T = erlang:universaltime(),
	B2 = update_rfc1123(B1, Prev, T),
	ets:insert(?MODULE, {rfc1123, B2}),
	TRef = erlang:send_after(1000, self(), update),
	{noreply, #state{universaltime=T, rfc1123=B2, tref=TRef}};
handle_info(_Info, State) ->
	{noreply, State}.
```

这个模块使用 handle_info/2 只有一个作用，就是用于更新时间，通过每次更新时间时调用 erlang:send_after(1000, self(), update) 相当于设置了一个 1000ms 的定时器，不断更新时间

可能是为了避免别的进程发消息到这个进程强制刷新时间，在 update 时，会把 erlang:send_after 返回的Ref保存在State中，每一次update时，都会把当前的Ref取消，以防进行多次不必要的刷新

其他回调函数中都没什么操作，就不解释了

State中也存储一份时间，用于更新时间的计算

内部方法
---

update_rfc1123/3 的匹配中，从上到下不同分支分别是为了适应没更新、更新秒、分、小时、天、月 的情况，更新年与初始化时，都调用通用匹配

这种做法可以将更新时间所带来的性能损耗降到最小

小结
---

其实这种时间的处理方式在我们游戏中也在使用，不过在细微处有些许不同，比如取时间调用的函数，定时器Ref的设置，ETS表的参数等，接下来尝试一个一个比较，总结